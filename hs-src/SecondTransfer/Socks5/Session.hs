{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving  #-}
module SecondTransfer.Socks5.Session (
                 tlsSOCKS5Serve
               , ConnectOrForward                                  (..)
               , Socks5ServerState
               , initSocks5ServerState
     ) where

import           Control.Concurrent
import qualified Control.Exception                                  as E
import           Control.Lens                                       ( makeLenses, (^.), set)

import qualified Data.ByteString                                    as B
import           Data.ByteString.Char8                              ( unpack,  pack)
import qualified Data.Attoparsec.ByteString                         as P
import qualified Data.Binary                                        as U
import qualified Data.Binary.Put                                    as U
import           Data.Word                                          (Word16)
import           Data.Int                                           (Int64)

import qualified Network.Socket                                     as NS

import           SecondTransfer.Exception                           (SOCKS5ProtocolException (..), forkIOExc )

import           SecondTransfer.Socks5.Types
import           SecondTransfer.Socks5.Parsers
import           SecondTransfer.Socks5.Serializers

import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.IOCallbacks.SocketServer
import           SecondTransfer.IOCallbacks.Coupling                (couple)
import           SecondTransfer.IOCallbacks.WrapSocket              (HasSocketPeer(..))

-- For debugging purposes
--import           SecondTransfer.IOCallbacks.Botcher


data Socks5ServerState = Socks5ServerState {
    _nextConnection_S5S       :: !Int64
    }
makeLenses ''Socks5ServerState

initSocks5ServerState :: Socks5ServerState
initSocks5ServerState = Socks5ServerState 0

data ConnectOrForward =
    Connect_COF B.ByteString IOCallbacks
  | Forward_COF B.ByteString Word16
  | Drop_COF B.ByteString


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
--   by the first parameter. Quite simple.
negotiateSocksAndForward ::  (B.ByteString -> Bool) -> IOCallbacks -> IO ConnectOrForward
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

                        -- CORRECT WAY:
                        return $ Connect_COF named_host socks_here

                    else do
                        -- Logging? We need to get that real right.
                        return $ Drop_COF named_host


            -- Other commands not handled for now
            _             -> do
                return $ Drop_COF "<socks5-unimplemented>"

    case ei of
        Left SOCKS5ProtocolException -> return $ Drop_COF "<protocol exception>"
        Right result -> return result


-- | Forwards a set of IOCallbacks (actually, it is exactly the same passed in) after the
--   SOCKS5 negotiation, if the negotiation succeeds and the indicated "host" is approved
--   by the first parameter. If the approver returns false, this function will try to
--   actually connect to the host and let the software act as a true proxy.
negotiateSocksForwardOrConnect ::  (B.ByteString -> Bool) -> IOCallbacks -> IO ConnectOrForward
negotiateSocksForwardOrConnect approver socks_here =
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
                    port_number = req_packet ^. port_SP3
                    externalConnectProcessing  =
                      do
                        maybe_forwarding_callbacks <- connectOnBehalfOfClient address port_number
                        case maybe_forwarding_callbacks of
                            Just (_indicated_address, io_callbacks) -> do
                                let
                                    server_reply =  ServerReply_Packet {
                                        _version_SP4    = ProtocolVersion
                                      , _replyField_SP4 = Succeeded_S5RF
                                      , _reservedField_SP4 = 0
                                      , _address_SP4 = IPv4_IA 0x7f000001
                                         -- Wrong port, but...
                                      , _port_SP4 = 10001
                                        }
                                ps putServerReply_Packet server_reply
                                -- Now couple the two streams ...
                                _ <- couple socks_here io_callbacks
                                return $ Forward_COF (pack . show $ address) (fromIntegral port_number)
                            _ ->
                                return $ Drop_COF (pack . show $ address)


                -- /let
                case address of
                    DomainName_IA named_host ->
                        if  approver named_host
                          then do
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
                            return $ Connect_COF named_host socks_here
                          else do
                            -- Forward to an external host
                            externalConnectProcessing
                    IPv4_IA _ -> do
                        -- TODO: Some address sanitization
                        externalConnectProcessing


            -- Other commands not handled for now
            _             -> do
                --putStrLn "SOCKS5 HAS NEGLECTED TO REJECT A CONNECTION"
                return $ Drop_COF "<socks5-unimplemented>"

    case ei of
        Left SOCKS5ProtocolException -> return $ Drop_COF "<socks5-protocol-exception>"
        Right result -> return result


connectOnBehalfOfClient :: IndicatedAddress -> Word16 -> IO (Maybe (IndicatedAddress , IOCallbacks))
connectOnBehalfOfClient address port_number =
  do
    maybe_sock_addr <- case address of
        IPv4_IA addr ->
            return . Just $  NS.SockAddrInet (fromIntegral port_number) addr

        DomainName_IA dn -> do
            -- Let's try to connect on behalf of the client...
            let
                hints = NS.defaultHints {
                    NS.addrFlags = [NS.AI_ADDRCONFIG]
                  }
            addrs <- E.catch
               ( NS.getAddrInfo (Just hints) (Just . unpack $ dn) Nothing )
               ((\_ -> return [])::E.IOException -> IO [NS.AddrInfo])
            case addrs of
                ( first : _) -> do
                    return . Just $ NS.addrAddress first
                _ ->
                    return Nothing

    case maybe_sock_addr of
        Just sock_addr@(NS.SockAddrInet _ ha) -> do
            E.catch
                (do
                    client_socket <-  NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
                    let
                        translated_address =  (NS.SockAddrInet (fromIntegral port_number) ha)
                    NS.connect client_socket translated_address
                    is_connected <- NS.isConnected client_socket
                    peer_name <- NS.getPeerName client_socket
                    socket_io_callbacks <- socketIOCallbacks client_socket
                    io_callbacks <- handshake socket_io_callbacks
                    return . Just $ (toSocks5Addr translated_address , io_callbacks)
                )
                ((\_ -> do
                    return Nothing)::E.IOException -> IO (Maybe (IndicatedAddress , IOCallbacks) ) )

        _ -> do
            -- Temporary message
            putStrLn "SOCKS5 could not be forwarded/address not resolved, or resolved to strange format"
            return Nothing


toSocks5Addr:: NS.SockAddr -> IndicatedAddress
toSocks5Addr (NS.SockAddrInet _ ha) = IPv4_IA ha
toSocks5Addr _                      = error "toSocks5Addr not fully implemented"


-- | Simple alias to SocketIOCallbacks where we expect
--   encrypted contents over a SOCKS5 Socket
newtype TLSServerSOCKS5Callbacks = TLSServerSOCKS5Callbacks SocketIOCallbacks

instance IOChannels TLSServerSOCKS5Callbacks where
    handshake (TLSServerSOCKS5Callbacks cb) = handshake cb

instance TLSEncryptedIO TLSServerSOCKS5Callbacks
instance TLSServerIO TLSServerSOCKS5Callbacks
instance HasSocketPeer TLSServerSOCKS5Callbacks where
    getSocketPeerAddress (TLSServerSOCKS5Callbacks s) = getSocketPeerAddress s


-- | tlsSOCKS5Serve approver listening_socket onsocks5_action
-- The approver should return True for host names that are served by this software (otherwise the connection will be closed, just for now,
-- in the close future we will implement a way to forward requests to external Internet hosts.)
-- Pass a bound and listening TCP socket where you expect a SOCKS5 exchange to have to tke place.
-- And pass an action that can do something with the callbacks. The passed-in action is expected to fork a thread and return
-- inmediately.
tlsSOCKS5Serve ::
    MVar Socks5ServerState
 -> Socks5ConnectionCallbacks
 -> (B.ByteString -> Bool)
 -> Bool
 -> NS.Socket
 -> ( TLSServerSOCKS5Callbacks -> IO () )
 -> IO ()
tlsSOCKS5Serve s5s_mvar socks5_callbacks approver forward_connections listen_socket onsocks5_action =
     tcpServe listen_socket socks_action
  where
     socks_action active_socket = do
         forkIOExc "tlsSOCKS5Serve/negotiation" $ do
             conn_id <- modifyMVar s5s_mvar $ \ s5s -> do
                 let
                     conn_id = s5s ^. nextConnection_S5S
                     new_s5s  = set nextConnection_S5S (conn_id + 1) s5s
                 return $ new_s5s `seq` (new_s5s, conn_id)
             let
                 log_events_maybe = socks5_callbacks ^. logEvents_S5CC
                 log_event :: Socks5ConnectEvent -> IO ()
                 log_event ev = case log_events_maybe of
                     Nothing -> return ()
                     Just c -> c ev
                 wconn_id = S5ConnectionId conn_id
             peer_address <- NS.getPeerName active_socket
             log_event $ Established_S5Ev peer_address wconn_id

             socket_io_callbacks <- socketIOCallbacks active_socket
             io_callbacks <- handshake socket_io_callbacks
             maybe_negotiated_io <-
               if forward_connections
                   then negotiateSocksForwardOrConnect approver io_callbacks
                   else negotiateSocksAndForward       approver io_callbacks
             case maybe_negotiated_io of
                 Connect_COF fate negotiated_io -> do
                     let
                         tls_server_socks5_callbacks = TLSServerSOCKS5Callbacks socket_io_callbacks
                     log_event $ HandlingHere_S5Ev fate wconn_id
                     onsocks5_action tls_server_socks5_callbacks
                 Drop_COF fate -> do
                     log_event $ Dropped_S5Ev fate wconn_id
                     (io_callbacks ^. closeAction_IOC)
                     return ()
                 Forward_COF fate port -> do
                     -- TODO: More data needs to come here
                     -- Do not close
                     log_event $ ToExternal_S5Ev fate port wconn_id
                     return ()
         return ()
