{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving  #-}
module SecondTransfer.Socks5.Session (
                 tlsSOCKS5Serve
     ) where

---import           Control.Concurrent
import qualified Control.Exception                                  as E
import           Control.Lens                                       ( {-makeLenses,-} (^.))

import qualified Data.ByteString                                    as B
import           Data.ByteString.Char8                              ( {-unpack, -} pack)
import qualified Data.Attoparsec.ByteString                         as P
import qualified Data.Binary                                        as U
import qualified Data.Binary.Put                                    as U

import qualified Network.Socket                                     as NS

import           SecondTransfer.Exception                           (SOCKS5ProtocolException (..) )

import           SecondTransfer.Socks5.Types
import           SecondTransfer.Socks5.Parsers
import           SecondTransfer.Socks5.Serializers

import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.IOCallbacks.SocketServer


-- data S5SessionState = SessionState {

--     }

tryRead :: IOCallbacks ->  B.ByteString  -> P.Parser a -> IO (a,B.ByteString)
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
               Right Nothing -> E.throwIO SOCKS5ProtocolException
               Left ff -> go ff

    case onResult $ P.parse p leftovers  of
        Right (Just x) -> return x
        Right Nothing  -> E.throwIO SOCKS5ProtocolException
        Left f0 -> go f0


pushDatum :: IOCallbacks -> (a -> U.Put) -> a -> IO ()
pushDatum iocallbacks putthing x = do
    let
        datum = U.runPut (putthing x)
    (iocallbacks ^. pushAction_IOC) datum


-- | Forwards a set of IOCallbacks (actually, it is exactly the same passed in) after the
--   SOCKS5 negotiation, if the negotiation succeeds and the indicated "host" is approved
--   by the first parameter
negotiateSocksAndForward ::  (B.ByteString -> Bool) -> IOCallbacks -> IO (Maybe IOCallbacks)
negotiateSocksAndForward approver socks_here =
  do
    let
        tr = tryRead socks_here
        ps = pushDatum socks_here
    -- Start by reading the standard socks5 header
    ei <- E.try $ do
        (_auth, next1) <- tr ""  parseClientAuthMethods_Packet
        -- I will ignore the auth methods for now
        let
            server_selects = ServerSelectsMethod_Packet ProtocolVersion 0 -- No auth
        ps putServerSelectsMethod_Packet server_selects
        (req_packet, _next2) <- tr next1 parseClientRequest_Packet
        case req_packet ^. cmd_SP3 of

            Connect_S5PC  -> do
                -- Can accept a connect, to what?
                let
                    address = req_packet ^. address_SP3
                    named_host = case address of
                        DomainName_IA name -> name
                        _  -> ""
                if  approver named_host then
                    do
                        -- First I need to answer to the client that we are happy and ready
                        let
                            server_reply = ServerReply_Packet {
                                _version_SP4    = ProtocolVersion
                              , _replyField_SP4 = Succeeded_S5RF
                              , _reservedField_SP4 = 0
                              , _address_SP4 = IPv4_IA 0x7f000001
                              , _port_SP4 = 10001
                                }
                        ps putServerReply_Packet server_reply
                        -- Now that I have the attendant, let's just activate it ...
                        return . Just $ socks_here
                    else do
                        -- Logging? We need to get that real right.
                        return Nothing


            -- Other commands not handled for now
            _             -> do
                return Nothing

    case ei of
        Left SOCKS5ProtocolException -> return Nothing
        Right result -> return result


-- | Simple alias to SocketIOCallbacks where we expect
--   encrypted contents over a SOCKS5 Socket
newtype TLSServerSOCKS5Callbacks = TLSServerSOCKS5Callbacks IOCallbacks

instance IOChannels TLSServerSOCKS5Callbacks where
    handshake (TLSServerSOCKS5Callbacks cb) = return cb

instance TLSEncryptedIO TLSServerSOCKS5Callbacks
instance TLSServerIO TLSServerSOCKS5Callbacks


-- | tlsSOCKS5Serve approver listening_socket onsocks5_action
-- The approver should return True for host names that are served by this software (otherwise the connection will be closed, just for now,
-- in the close future we will implement a way to forward requests to external Internet hosts.)
-- Pass a bound and listening TCP socket where you expect a SOCKS5 exchange to have to tke place.
-- And pass an action that can do something with the callbacks. The passed-in action is expected to fork a thread and return
-- inmediately.
tlsSOCKS5Serve :: (B.ByteString -> Bool)  -> NS.Socket -> ( TLSServerSOCKS5Callbacks -> IO () ) -> IO ()
tlsSOCKS5Serve approver listen_socket onsocks5_action =
     tcpServe listen_socket socks_action
  where
     socks_action active_socket = do
         socket_io_callbacks <- socketIOCallbacks active_socket
         io_callbacks <- handshake socket_io_callbacks
         maybe_negotiated_io <- negotiateSocksAndForward approver io_callbacks
         case maybe_negotiated_io of
             Just negotiated_io ->
                 let
                     tls_server_socks5_callbacks = TLSServerSOCKS5Callbacks negotiated_io
                 in  onsocks5_action tls_server_socks5_callbacks

             Nothing -> do
                 (io_callbacks ^. closeAction_IOC)
                 return ()
