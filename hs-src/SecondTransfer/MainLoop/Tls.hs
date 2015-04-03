-- TLS is small hell to setup, let's do it here ...
{-# LANGUAGE StandaloneDeriving, FlexibleInstances, DeriveDataTypeable #-}
module SecondTransfer.MainLoop.Tls(readyTCPSocket
    , tcpServe
    , enchantSocket
    , buildContextParams
    , tlsServe
    , tlsServeProtocols 
    ) where 


import           Network.Socket 
import           Control.Concurrent
import qualified Crypto.PubKey.DH     as DH
import qualified Data.ByteString      as B
import           Data.Default         (def)
import           Data.Typeable        (Typeable)
import           Data.X509            (decodeSignedCertificate)
import qualified Network.TLS          as T
import           System.FilePath      ((</>))
import           Crypto.Random
import           Network.TLS.Extra.Cipher (ciphersuite_all)
import           System.Exit
import           System.Posix.Signals
import qualified Control.Exception    as E
import           Data.ByteString.Char8(pack, unpack)
import           Data.List   (find)


import SecondTransfer.MainLoop.ConfigHelp(configDir)
import SecondTransfer.MainLoop.PushPullType


data CouldNotDecideProtocol = CouldNotDecideProtocol
    deriving (Show, Typeable)

instance E.Exception CouldNotDecideProtocol


readyTCPSocket :: String -> Int ->  IO Socket 
readyTCPSocket hostname portnumber = do 
    the_socket <- socket AF_INET Stream defaultProtocol 
    addr_info0 : _ <- getAddrInfo Nothing (Just hostname) Nothing
    addr_info1  <- return $ addr_info0 {
        addrFamily = AF_INET
        ,addrSocketType = Stream
        ,addrAddress = (\ (SockAddrInet _ a) -> SockAddrInet (fromIntegral portnumber) a) 
                       (addrAddress addr_info0) 
        }
    host_address <- return $ addrAddress addr_info1
    setSocketOption the_socket ReusePort 1
    setSocketOption the_socket RecvBuffer 32000
    setSocketOption the_socket SendBuffer 32000
    setSocketOption the_socket RecvLowWater 8
    bindSocket the_socket host_address
    return the_socket


tcpServe :: Socket -> ( Socket -> IO () ) -> IO ()
tcpServe  to_bind_socket action =  
    do 
        listen to_bind_socket 10000
        accept_loop to_bind_socket 
  where 
    accept_loop bind_socket = do 
        (new_socket, _ ) <- accept bind_socket
        E.catch
            (do 
                action new_socket
            )
            ( (\ e -> putStrLn $ show e) :: E.SomeException -> IO ())
        accept_loop bind_socket 


buildContextParams :: FilePath -> IO T.ServerParams 
buildContextParams base_dir = do 
    let config_dir = configDir base_dir

    -- CA certificates
    cacert_fs_path <- return $ config_dir </> "ca/cacert.der" 
    cacert_str <- B.readFile cacert_fs_path  
    (Right casigned_certificate) <- return $ decodeSignedCertificate cacert_str 

    -- My credential
    (Right credential) <- T.credentialLoadX509 
        (config_dir </> "servercert.pem")
        (config_dir </> "privkey.pem")

    return $ def {
        T.serverCACertificates = [ casigned_certificate ]
        ,T.serverWantClientCert = False 
        ,T.serverDHEParams = Just dhParams 
        ,T.serverShared = def {
            T.sharedCredentials = T.Credentials [ credential]
        }
        --,T.serverHooks = _sh
        ,T.serverSupported = serverSupported
        }


buildNPNContextParams            :: FilePath -> [String] -> IO T.ServerParams 
buildNPNContextParams base_dir protocols  = do 
    let config_dir = configDir base_dir

    -- CA certificates
    cacert_fs_path <- return $ config_dir </> "ca/cacert.der" 
    cacert_str <- B.readFile cacert_fs_path  
    (Right casigned_certificate) <- return $ decodeSignedCertificate cacert_str 

    -- My credential
    (Right credential) <- T.credentialLoadX509 
        (config_dir </> "servercert.pem")
        (config_dir </> "privkey.pem")

    return $ def {
        T.serverCACertificates = [ casigned_certificate ]
        ,T.serverWantClientCert = False 
        ,T.serverDHEParams = Just dhParams 
        ,T.serverShared = def {
            T.sharedCredentials = T.Credentials [ credential]
        }
        ,T.serverHooks = def {
            T.onSuggestNextProtocols = do
                -- putStrLn "Protocols asked"
                return $ Just protocols_as_bs
            ,T.onALPNClientSuggest = Just $ \ suggested_protocolls -> do 
                putStrLn $ "Client suggests through ALPN: " ++ (show suggested_protocolls)
                chooseFirstMatching protocols_as_bs suggested_protocolls
        }
        ,T.serverSupported = serverSupported
        }

  where 
    protocols_as_bs =  map pack protocols


chooseFirstMatching :: [B.ByteString] -> [B.ByteString] -> IO B.ByteString
chooseFirstMatching [] _                                                        = E.throwIO CouldNotDecideProtocol
chooseFirstMatching (p:_) suggested_protocolls | p `elem` suggested_protocolls = return p 
chooseFirstMatching (_:xs) suggested_protocolls                                 = chooseFirstMatching xs suggested_protocolls


serverSupported :: T.Supported
serverSupported = def {
    T.supportedVersions = [T.TLS12]
    ,T.supportedCiphers =  filter
        (T.cipherAllowedForVersion T.TLS12)
        ciphersuite_all
    }


-- Wired DH params....
--- TODO: Fix here 
dhParams :: DH.Params
dhParams = DH.Params{ 
    DH.params_p = 160870573327511515755694812153035117423465767994224171293799401008595077500999512573342274297754048601232162928745909141418010838816767337684899532156625070107178016199211264183981876937379840682650214449906599372700826256634113979846962538158218926053405148278109540762210692241689283056010768548464169065243
    ,DH.params_g = 2 
    }
   

enchantSocket :: Socket -> T.ServerParams -> IO T.Context
enchantSocket s server_params = do  
    rng <- rngProduce
    ctx <- T.contextNew s server_params rng
    T.handshake ctx
    return ctx


rngProduce :: IO SystemRNG
rngProduce = do 
    rng_pool <- createEntropyPool 
    return $ cprgCreate rng_pool


-- Serves always the same protocol in the session
tlsServe :: FilePath -> (PushAction -> PullAction -> IO () ) -> String -> Int -> IO ()
tlsServe base_path session_attendant interface_name interface_port = do 
    tid <- myThreadId
    installHandler keyboardSignal (Catch (do 
      E.throwTo tid ExitSuccess
      )) Nothing
    listening_socket <- readyTCPSocket interface_name interface_port
    server_params <- buildContextParams base_path
    tcpServe listening_socket $ \ s -> do
      -- Seems like a good place to move stuff to a thread... 
      ctx <- enchantSocket s server_params
      -- Time to say something
      rd <- return $ (T.recvData ctx) 
      sd <- return $ (T.sendData ctx)
      session_attendant sd rd
      T.bye ctx


-- | After the Next-Protocol-Negotiation (or its current counter-part), 
--   this function decides who is going to take care of the serving the 
--   protocol. A greenlet thread is forked to take care of it.
--   First argument is a list of protocol names and the Attendant functions. 
--   The second argument is the IP address of the network interface where
--   the TLS socket will listen. The third argument is the port number of 
--   said socket.
tlsServeProtocols :: FilePath -> [ (String, Attendant) ] -> String -> Int -> IO ()
tlsServeProtocols base_path attendants interface_name interface_port = do  
    tid <- myThreadId
    installHandler keyboardSignal (Catch (do 
      E.throwTo tid ExitSuccess
      )) Nothing
    listening_socket <- readyTCPSocket interface_name interface_port
    server_params <- buildNPNContextParams base_path $ map fst attendants
    tcpServe listening_socket $ \ s -> do 
        
        ctx <- enchantSocket s server_params
        
        selected_protocol_maybe <- T.getNegotiatedProtocol ctx
        putStrLn $ "Protocol selected: " ++ (show selected_protocol_maybe)
        
        session_attendant <- return $ case selected_protocol_maybe of 
            Just next_protocol_bs -> let 
                next_protocol = unpack next_protocol_bs
                Just (_, attendant) = find 
                     ( \ (p,_) -> p == next_protocol) 
                     attendants
              in attendant 
            Nothing  -> (snd . head) attendants
      
        rd <- return $ (T.recvData ctx) 
        sd <- return $ (T.sendData ctx)


        forkIO $ do 
             session_attendant sd rd 
             putStrLn "Session attendant off"
             threadDelay 1000000
             T.bye ctx
             sClose s
             putStrLn "Socket closed"
        return ()