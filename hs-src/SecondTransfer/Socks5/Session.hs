{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving  #-}
module SecondTransfer.Socks5.Session (
                 serveSocks
     ) where

---import           Control.Concurrent
-- import qualified Control.Exception                                  as E
import           Control.Lens                                       ( {-makeLenses,-} (^.))
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class                          (lift)
import           Control.Monad.IO.Class                             (liftIO)

import qualified Data.ByteString                                    as B
import           Data.ByteString.Char8                              ( {-unpack, -} pack)
import qualified Data.Attoparsec.ByteString                         as P
import qualified Data.Binary                                        as U
import qualified Data.Binary.Put                                    as U


import           SecondTransfer.Socks5.Types
import           SecondTransfer.Socks5.Parsers
import           SecondTransfer.Socks5.Serializers

import           SecondTransfer.IOCallbacks.Types
--import           SecondTransfer.IOCallbacks.Coupling


-- data S5SessionState = SessionState {

--     }

tryRead :: IOCallbacks ->  B.ByteString  -> P.Parser a -> IO  (a,B.ByteString)
tryRead iocallbacks leftovers p = do
    let
        onResult result =
            case result of
                P.Done i r   ->  Right . Just $ (r,i)
                P.Fail _ _ _ ->  Right $ Nothing
                P.Partial f  ->  Left $ f

        go  f = do
            fragment <- (iocallbacks ^. bestEffortPullAction_IOC) True
            case onResult $ f fragment of
               Right (Just x) -> return x
               Right Nothing -> fail "NoAlt"
               Left ff -> go ff

    case onResult $ P.parse p leftovers  of
        Right (Just x) -> return x
        Right Nothing  -> fail "NoAlt"
        Left f0 -> go f0


pushDatum :: IOCallbacks -> (a -> U.Put) -> a -> IO ()
pushDatum iocallbacks putthing x = do
    let
        datum = U.runPut (putthing x)
    (iocallbacks ^. pushAction_IOC) datum


-- | Serves a sock proxy in the given IOCallbacks channel. This function does its work
--   in the calling thread, but it returns as soon as work is delegated to the attendant
--   returned by the resolver.
serveSocks :: (Socks5Resolver a) =>  a -> IOCallbacks -> IO (Maybe MonoDisruptible)
serveSocks resolver socks_here = runMaybeT $ do
    let
        tr = tryRead socks_here
        ps = pushDatum socks_here
    -- Start by reading the standard socks5 header
    (_auth, next1) <- lift $ tr ""  parseClientAuthMethods_Packet
    -- I will ignore the auth methods for now
    let
        server_selects = ServerSelectsMethod_Packet ProtocolVersion 0 -- No auth
    liftIO $ ps putServerSelectsMethod_Packet server_selects
    (req_packet, next2) <- lift $ tr next1 parseClientRequest_Packet
    case req_packet ^. cmd_SP3 of

        Connect_S5PC  -> do
            -- Can accept a connect, to what?
            let
                address = req_packet ^. address_SP3
                port_no = fromIntegral $ req_packet ^. port_SP3
            maybe_attendant <- lift . s5Resolve resolver address $ port_no
            case maybe_attendant of

                Just attendant -> do
                    -- First I need to answer to the client that we are happy and ready
                    let
                        server_reply = ServerReply_Packet {
                            _version_SP4    = ProtocolVersion
                          , _replyField_SP4 = Succeeded_S5RF
                          , _reservedField_SP4 = 0
                          , _address_SP4 = IPv4_IA 0x7f000001
                          , _port_SP4 = 10001
                            }
                    liftIO $ ps putServerReply_Packet server_reply
                    -- Should I create an IOCallbacks that can take into account any unprocessed information from the browser?
                    -- Like next2?
                    liftIO . B.putStr $ "Excedent:::: " `mappend` next2 `mappend` " (length " `mappend` (pack . show . B.length $ next2) `mappend`  ")\n"
                    -- Now that I have the attendant, let's just activate it ...
                    interruptible <- lift . attendant $ socks_here
                    return interruptible


        -- Other commands not handled for now
        _             -> do
            -- Remove this trace!!
            liftIO $ putStrLn "Invalid command"
            fail "Invalid command"
