{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Rede.Research.ResearchWorker(
    runResearchWorker
    ,spawnHarServer
    ) where 


import           Control.Lens                    ((^.), (%~), (.~))
import qualified Control.Lens                    as L
-- import           Control.Exception              (catch)
import           Control.Concurrent              (forkIO, threadDelay)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad.Catch             (catch)

import qualified Data.ByteString                 as B
import qualified Data.ByteString.Builder         as Bu
import           Data.ByteString.Char8           (pack, unpack)
import qualified Data.ByteString.Lazy.Char8      as Lch
import qualified Data.ByteString.Lazy            as LB
import qualified Data.HashTable.IO               as H
import           Data.Conduit
import           Data.Foldable                   (foldMap)
import           Data.Monoid                     (mappend)
import qualified Data.Monoid                     as M
-- import qualified Data.ByteString.Lazy           as LB

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader      (ReaderT (..), runReaderT)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

import qualified Network.URI                     as U

import           System.Log.Logger
import           Text.Printf                     (printf)

import           Rede.Workers.ResponseFragments  (RequestMethod (..),
                                                  getMethodFromHeaders,
                                                  getUrlFromHeaders,
                                                  simpleResponse)

import           Rede.HarFiles.DnsMasq           (dnsMasqFileContentsToIp)
-- import           Rede.HarFiles.JSONDataStructure (originUrl)
import           Rede.HarFiles.ServedEntry       (BadHarFile (..),
                                                  ResolveCenter, allSeenHosts,
                                                  rcName, resolveCenterFromLazyByteString,
                                                  rcOriginalUrl
                                                  )
import           Rede.Http2.MakeAttendant        (http2Attendant)
import           Rede.MainLoop.CoherentWorker
import           Rede.MainLoop.ConfigHelp        (configDir, getInterfaceName,
                                                  getMimicPort)
import           Rede.MainLoop.OpenSSL_TLS       (FinishRequest (..), tlsServeWithALPNAndFinishOnRequest)
import           Rede.Utils.ConcatConduit        (concatConduit)
import           Rede.Utils.PrintfArgByteString  ()
import           Rede.Utils                      (hashSafeFromUrl, SafeUrl, unSafeUrl)
import           Rede.Utils.Alarm                
import           Rede.Workers.HarWorker          (harCoherentWorker)


type HashTable k v = H.CuckooHashTable k v


data CurrentAnalysisStage = 
    AnalysisRequested_CAS
    |SentToHarvester_CAS
    |SentToTest_CAS
    |SystemCancelled_CAS
    |Done_CAS


-- Time to wait before unleashing an alarm... 
timeForAlarm :: Int 
timeForAlarm = 40000000


-- This state structure follows a given URL analysis. 
-- You can find a dictionary of these in the structure below...
data UrlState = UrlState {
    _completeUrl     :: B.ByteString
    ,_analysisStage  :: CurrentAnalysisStage
    ,_currentAlarm   :: Alarm ()
    }


L.makeLenses ''UrlState


data ServiceState = ServiceState {
    -- Put one here, let it run through the pipeline...
    _nextHarvestUrl :: Chan B.ByteString

    -- To be passed on to StationB
    ,_nextTestUrl :: Chan B.ByteString

    -- Files to be handed out to DNSMasq
    , _nextDNSMasqFile :: Chan B.ByteString

    , _resolveCenterChan :: Chan ResolveCenter

    , _finishRequestChan :: Chan FinishRequest

    , _researchDir :: FilePath 

    -- The IP address that StationB needs to connect
    , _useAddressForStationB :: B.ByteString

    -- The state of each URL 
    , _urlState :: HashTable SafeUrl UrlState

    -- This one we use in the opposite sense: we write here...
    -- ,_servingHar    :: Chan (ResolveCenter, OriginUrl )
    }

L.makeLenses ''ServiceState


type ServiceStateMonad = ReaderT ServiceState IO


runResearchWorker :: 
    Chan B.ByteString 
    -> Chan ResolveCenter 
    -> Chan FinishRequest 
    -> FilePath
    -> B.ByteString
    -> IO CoherentWorker
runResearchWorker url_chan resolve_center_chan finish_request_chan research_dir use_address_for_station_b = do 
    liftIO $ infoM "ResearchWorker" "Starting research worker"
    next_test_url_chan <- newChan 
    next_dns_masq_file <- newChan
    new_url_state <- H.new

    let     
        state = ServiceState {
             _nextHarvestUrl = url_chan
            ,_nextTestUrl    = next_test_url_chan
            ,_nextDNSMasqFile = next_dns_masq_file
            ,_resolveCenterChan = resolve_center_chan
            ,_finishRequestChan = finish_request_chan
            ,_researchDir = research_dir
            ,_useAddressForStationB = use_address_for_station_b
            ,_urlState = new_url_state
            }
    return $ \request -> runReaderT (researchWorkerComp request) state


researchWorkerComp :: Request -> ServiceStateMonad PrincipalStream
researchWorkerComp (input_headers, maybe_source) = do 
    next_harvest_url              <- L.view nextHarvestUrl
    next_dnsmasq_chan             <- L.view nextDNSMasqFile
    next_test_url_chan            <- L.view nextTestUrl
    resolve_center_chan           <- L.view resolveCenterChan 
    finish_chan                   <- L.view finishRequestChan 
    base_research_dir             <- L.view researchDir
    use_address_for_station_b     <- L.view useAddressForStationB
    url_state_hash                <- L.view urlState

    let 
        method = getMethodFromHeaders input_headers
        req_url  = getUrlFromHeaders input_headers

    case method of 

        -- Most requests are served by POST to emphasize that a request changes 
        -- the state of this program... 
        Post_RM 
            | req_url == "/nexturl/" -> do 
                -- Starts harvesting of a resource... this request is made by StationA
                liftIO $ infoM "ResearchWorker" ".. /nexturl/ asked "
                url <- liftIO $ readChan next_harvest_url
                liftIO $ infoM "ResearchWorker" .  builderToString $ "..  /nexturl/" `mappend` " answer " `mappend` (Bu.byteString url) `mappend` " "

                -- Mark it as sent...
                modifyUrlState (hashSafeFromUrl url) $ \ old_state -> do 
                    -- Set an alarm here also...this alarm should be disabled when a .har file comes....
                    liftIO $ cancelAlarm $ old_state ^. currentAlarm
                    let 
                        a1 = analysisStage .~ SentToHarvester_CAS $ old_state
                    alarm <- liftIO $ newAlarm timeForAlarm $ do 
                        errorM "ResearchWorker" . builderToString $ " failed harvesting " `mappend` (Bu.byteString req_url) `mappend` " (timeout) "
                    let
                        a2 = currentAlarm .~ alarm $ a1
                    return a2 
                

                return $ simpleResponse 200 url 

            | req_url == "/testurl/" -> do 
                -- Sends the next test url to the Chrome extension, this starts processing by 
                -- StationB... the request is started by StationB and we send the url in the response...
                liftIO $ infoM "ResearchWorker" "..  /testurl/ was asked"
                url <- liftIO $ readChan next_test_url_chan
                -- Okej, got the thing, but I' need to be sure that there has been enough time to 
                -- apply the new network settings in StationB 
                liftIO $ threadDelay 4000000 -- <-- 4 seconds
                liftIO $ infoM "ResearchWorker" . builderToString $ "..  /testurl/ answered with " `mappend` (Bu.byteString url)
                modifyUrlState (hashSafeFromUrl url) $ \ old_state -> do 
                    -- This alarm should be disabled by the http2har file
                    liftIO $ cancelAlarm $ old_state ^. currentAlarm
                    let 
                        a1 = analysisStage .~ SentToTest_CAS $ old_state
                    alarm <- liftIO $ newAlarm 15000000 $ do 
                        errorM "ResearchWorker" . builderToString $ " failed testing " `mappend` (Bu.byteString url)
                    return $ currentAlarm .~ alarm $ a1
                return $ simpleResponse 200 url

            | req_url == "/har/", Just source <- maybe_source -> do

                -- Receives a .har object from the harvest station ("StationA"), and 
                -- makes all the arrangements for it to be tested using HTTP 2
                liftIO $ infoM "ResearchWorker" "..  /har/"
                catch 
                    (do
                        (resolve_center, har_file_contents) <- liftIO $ output_computation source
                        let 
                            test_url = resolve_center ^. rcOriginalUrl
                            use_text = "Response processed"

                        -- Okey, cancel the alarm first 
                        modifyUrlState  (hashSafeFromUrl test_url) $ \ old_state -> do
                            liftIO $ cancelAlarm $ old_state ^. currentAlarm
                            alarm <- liftIO $ newAlarm 15000000 $ do 
                                errorM "ResearchWorker" . builderToString $ " failed forwarding " `mappend` (Bu.byteString test_url)
                            return $ currentAlarm .~ alarm $ old_state
                        

                        -- Create the contents to go in the dnsmasq file, and queue them 
                        let 
                            all_seen_hosts = resolve_center ^. allSeenHosts
                            dnsmasq_contents = dnsMasqFileContentsToIp use_address_for_station_b all_seen_hosts 

                        liftIO $ writeChan next_dnsmasq_chan dnsmasq_contents

                        -- We also need to queue the url somewhere to be used by StationB
                        liftIO $ writeChan next_test_url_chan $ urlToHTTPSScheme test_url

                        -- And the resolve center to be used by the serving side
                        liftIO $ writeChan resolve_center_chan resolve_center

                        -- We can also use this opportunity to create a folder for this research 
                        -- project 
                        let 
                            scratch_folder = (base_research_dir) </> (resolve_center ^. rcName . L.to unpack)
                            har_filename = scratch_folder </> "harvested.har"
                        liftIO $ createDirectoryIfMissing True scratch_folder
                        liftIO $ LB.writeFile har_filename har_file_contents

                        return $ simpleResponse 200 use_text
                    )
                    on_bad_har_file

            | req_url == "/http2har/", Just source <- maybe_source -> do
                -- Receives a .har object from the harvest station ("StationA"), and 
                -- makes all the arrangements for it to be tested using HTTP 2
                liftIO $ infoM "ResearchWorker" "..  /http2har/"
                catch 
                    (do
                        (resolve_center,  har_contents_lb) <- liftIO $ output_computation source
                        let 
                            test_url = resolve_center ^. rcOriginalUrl
                            use_text = "Response processed"

                        let 
                            scratch_folder = base_research_dir </> (unpack (resolve_center ^. rcName ) )
                            har_filename = scratch_folder </> "test_http2.har"
                        liftIO $ LB.writeFile har_filename har_contents_lb


                        -- Okey, cancel the alarm  
                        modifyUrlState  (hashSafeFromUrl test_url) $ \ old_state -> do
                            liftIO $ cancelAlarm $ old_state ^. currentAlarm
                            return old_state

                        -- And finish the business
                        liftIO $ writeChan finish_chan FinishRequest

                        return $ simpleResponse 200 use_text
                    )
                    on_bad_har_file

            | req_url == "/dnsmasq/" -> do 
                -- Sends a DNSMASQ file to the test station ("StationB")
                liftIO $ infoM "ResearchWorker" ".. /dnsmasq/ asked"
                -- Serve the DNS masq file corresponding to the last .har file 
                -- received.
                dnsmasq_contents <- liftIO $ readChan next_dnsmasq_chan
                liftIO $ infoM "ResearchWorker" ".. /dnsmasq/ answer"
                
                return $ simpleResponse 200 dnsmasq_contents

            | req_url == "/setnexturl/", Just source <- maybe_source -> do 
                -- Receives a url to investigate from the user front-end, or from curl
                url_to_analyze <- liftIO $ source $$ concatConduit
                alarm <- liftIO $ newAlarm 15000000 $ do 
                    warningM "ResearchWorker" . builderToString $ "Job for url " `mappend` (Bu.byteString url_to_analyze) `mappend` " still waiting... "
                let 
                    job_descriptor = hashSafeFromUrl url_to_analyze
                    new_url_state = UrlState url_to_analyze AnalysisRequested_CAS alarm
                liftIO $ H.insert 
                    url_state_hash job_descriptor new_url_state 
                liftIO $ infoM "ResearchWorker" . builderToString $ ".. /setnexturl/ asked " `mappend` (Bu.byteString url_to_analyze)
                liftIO $ writeChan next_harvest_url url_to_analyze

                return $ simpleResponse 200 (unSafeUrl job_descriptor)

            | otherwise     -> do 
                return $ simpleResponse 500 "Can't handle url and method"

        _ -> do 
                return $ simpleResponse 500 "Can't handle url and method"


  where 
    on_bad_har_file :: BadHarFile -> ServiceStateMonad PrincipalStream
    on_bad_har_file  (BadHarFile _) = 
        return $ simpleResponse 500 "BadHarFile"

    output_computation :: InputDataStream -> IO (ResolveCenter,  LB.ByteString)
    output_computation source = do 
        full_builder <- source $$ consumer ""
        let
            lb  = Bu.toLazyByteString full_builder
            rc = resolveCenterFromLazyByteString lb
        return (rc, lb)
    consumer  b = do 
        maybe_bytes <- await 
        case maybe_bytes of 

            Just bytes -> do
                -- liftIO $ putStrLn $ "Got bytes " ++ (show $ B.length bytes)
                consumer $ b `M.mappend` (Bu.byteString bytes)

            Nothing -> do
                -- liftIO $ putStrLn "Finishing"
                return b


-- And here for when we need to fork a server to load the .har file from 
spawnHarServer :: FilePath -> Chan ResolveCenter -> Chan  FinishRequest -> IO ()
spawnHarServer mimic_dir resolve_center_chan finish_request_chan = do 
    let 
        mimic_config_dir = configDir mimic_dir    
    port  <-  getMimicPort
    infoM "ResearchWorker.SpawnHarServer"  $ ".. Mimic port: " ++ (show port)
    iface <-  getInterfaceName mimic_config_dir
    infoM "ResearchWorker.SpawnHarServer" $ ".. Mimic using interface: " ++ (show iface)


    finish_request_mvar <- newEmptyMVar 

    let 
        watcher = do 
            r <- readChan finish_request_chan
            infoM "ResearchWorker.SpawnHarServer" $ " .. Finishing mimic service  " 
            putMVar finish_request_mvar r
            watcher 

    forkIO watcher

    let 
        serveWork = do

            resolve_center <- readChan resolve_center_chan

            infoM "ResearchWorker.SpawnHarServer" $ printf ".. START for resolve center %s" (resolve_center ^. rcName)

            let 
                har_filename = unpack $ resolve_center ^. rcName 
                priv_key_filename = privKeyFilename mimic_dir har_filename
                cert_filename  = certificateFilename mimic_dir har_filename
                host_list = resolve_center ^. allSeenHosts

            infoM "ResearchWorker.SpawnHarServer" $ ".. .. about to create comprhensive certificate for " ++ har_filename
            getComprehensiveCertificate mimic_dir har_filename host_list

            infoM "ResearchWorker.SpawnHarServer" $ ".. .. .. Chosen cert. at file: " ++ (show cert_filename)
            infoM "ResearchWorker.SpawnHarServer" $ ".. .. .. ..  with private key: " ++ (show priv_key_filename)
            infoM "ResearchWorker.SpawnHarServer" $ ".. Starting mimic server"

            let 
                http2worker = harCoherentWorker resolve_center
            tlsServeWithALPNAndFinishOnRequest  cert_filename priv_key_filename iface [ 
                 -- ("h2-14", wrapSession veryBasic)
                 ("h2-14", http2Attendant http2worker)

                 -- TODO: Let the user select HTTP/1.1 from time to time...
                ] port finish_request_mvar
            -- 
            infoM "ResearchWorker.SpawnHarServer" $ printf ".. FINISH for resolve center %s" (resolve_center ^. rcName)
            serveWork 

    serveWork


certificateFilename :: FilePath -> FilePath -> FilePath 
certificateFilename mimic_dir harfilename = let
    basename = harfilename
    namesprefix = mimic_dir </> "fakecerts" </> basename
  in 
    namesprefix </> "server.pem" 


privKeyFilename :: FilePath -> FilePath -> FilePath 
privKeyFilename mimic_dir harfilename = let
    basename = harfilename
    namesprefix = mimic_dir </> "fakecerts" </> basename
  in 
    namesprefix </> "privkey.pem" 


getComprehensiveCertificate :: FilePath -> FilePath -> [B.ByteString] -> IO ()
getComprehensiveCertificate mimic_dir harfilename all_seen_hosts = do 

    let 
        basename = harfilename
        mimic_config_dir = mimic_dir </> "config"
        namesprefix = mimic_dir </> "fakecerts" </> basename

    createDirectoryIfMissing True namesprefix
    let 
        ca_location       = mimic_config_dir </> "ca"
        cnf_filename      = ca_location      </> "openssl-noprompt.cnf"
        db_filename       = ca_location      </> "certindex.txt"
        ca_cert           = ca_location      </> "cacert.pem"
        csr_filename      = namesprefix      </> "cert.csr"
        template_cnf      = namesprefix      </> "openssl.conf" 
        priv_key_filename = privKeyFilename  mimic_dir harfilename
        cert_filename     = namesprefix      </> "server.pem"

    -- Create a food file 
    templatefile <- B.readFile cnf_filename

    let 
        builder0 = Bu.byteString templatefile
        builderM1 = Bu.byteString $ "dir =" `mappend` (pack ca_location) `mappend` "\n\n\n"
        segment_builder = foldMap ( \ (hostname,i) -> 
                    Bu.byteString $ B.concat ["DNS.", (pack $ show i), "=", hostname, "\n"]
                ) all_seen_hosts_and_indices
        all_seen_hosts_and_indices = zip all_seen_hosts [2 .. ] :: [(B.ByteString,Int)]
        complete_builder = builderM1 `mappend` builder0 `mappend` segment_builder

    -- Save it 
    withFile template_cnf WriteMode $ \ handle -> 
        Bu.hPutBuilder handle complete_builder

    -- Invoke openssl to create a signing request
    (_, _, _, h ) <- createProcess $ proc "openssl" 
        ["req", 
            "-outform" , "PEM", 
            "-batch"   ,
            "-new"     , 
            "-newkey"  , "rsa:2048",
            "-nodes"   ,
            "-keyout"  ,  priv_key_filename,
            "-out"     , csr_filename,
            "-config"  , template_cnf
        ]
    waitForProcess h 

    -- Restart the database....
    B.writeFile db_filename ""

    -- Invoke to sign the certificate...
    (_, _, _, h2 ) <- createProcess $ proc "openssl"
        [ 
              "ca"
            , "-config", template_cnf
            , "-in"    , csr_filename
            , "-out"   , cert_filename
            , "-batch"
            , "-cert"  , ca_cert
        ]
    waitForProcess h2

    return ()    


urlToHTTPSScheme :: B.ByteString -> B.ByteString
urlToHTTPSScheme original_url = let 
    Just (U.URI {
        U.uriScheme    = _,
        U.uriPath      = uri_path,
        U.uriQuery     = uri_query,
        U.uriAuthority = uri_authority,
        U.uriFragment  = uri_fragment
        }) = U.parseURI $ unpack original_url
  in 
    pack $ show $ U.URI {
        U.uriScheme   = "https:",
        U.uriPath     = uri_path,
        U.uriQuery    = uri_query,
        U.uriFragment = uri_fragment,
        U.uriAuthority = uri_authority
        }


builderToString :: Bu.Builder -> String 
builderToString =  Lch.unpack . Bu.toLazyByteString


modifyUrlState :: SafeUrl -> (UrlState -> ServiceStateMonad UrlState) -> ServiceStateMonad ()
modifyUrlState key mutator = do 
    h  <- L.view urlState
    vv <- liftIO $ H.lookup h key
    case vv  of 
        Just v -> do 
            new_value <- mutator v 
            liftIO $ H.insert h key new_value

        Nothing -> do
            liftIO $ warningM "ResearchWorker" $  builderToString $ "Trying to modify state of non-seen url job " `mappend` (Bu.byteString . unSafeUrl $ key)

