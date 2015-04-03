{-# LANGUAGE OverloadedStrings 
            , GeneralizedNewtypeDeriving
            , TypeSynonymInstances
            , FlexibleInstances 
            , MultiParamTypeClasses
            #-}

module SecondTransfer.SpdyProtocol.TrivialTestWorker(
    trivialWorker
    ,fsWorker
    ,FsWorkerServicePocket
    ,FsWorkerSessionPocket
    ) where


import           Control.Monad.IO.Class
import qualified Data.ByteString            as B
import           Data.ByteString.Char8      (pack, unpack)
import           Data.Conduit
import           Data.List                  (find, isInfixOf, isSuffixOf)
import qualified Network.URI                as U
import           System.Directory           (doesFileExist)
import           System.FilePath
import qualified Data.HashTable.IO          as H
-- import           Control.Monad.Trans.Reader
import           Control.Concurrent.MVar
-- import           Control.Monad.Morph        (embed)
   


import           SecondTransfer.MainLoop.ConfigHelp
import           SecondTransfer.MainLoop.Tokens       (StreamInputToken       (..)
                                             ,StreamOutputAction    (..)
                                             ,StreamWorker
                                             ,UnpackedNameValueList (..)
                                             ,StreamWorkerClass     (..)
                                             
                                             ,getHeader)
import           SecondTransfer.SimpleHTTP1Response   (shortResponse)



-- Just so you remember:
-- type StreamWorker = Conduit StreamInputToken IO StreamOutputAction


-- | Simple worker that always sends the same answer
trivialWorker :: StreamWorker
trivialWorker = do 
    input_token_maybe <- await 
    case input_token_maybe of
        Nothing     ->  do 
            return ()

        Just (Headers_STk _) -> do
            yield $ SendHeaders_SOA $ UnpackedNameValueList  [
                 (":status", "200")
                ,(":version", "HTTP/1.1")
                ,("content-length", (pack.show $ B.length shortResponse))
                ,("content-type", "text/html")
                ,("server", "ReHv0.0")
                ]
            yield $ SendData_SOA shortResponse
            yield $ Finish_SOA


--  Here we start the filesystem worker, that keeps a very simple cache of resources ---

data FsWorkerSessionPocket  = FsWorkerSessionPocket {
    -- The first request on a session can be considered a "head"
    -- This is of course a bug and a very bad approach, but it will do it 
    -- for now
    sessionIsNew :: MVar Bool

    -- Are we recording something? This is only activated on new sessions,
    -- so we have some security about not having many fetches after this 
    -- point
    ,sessionIsRecording :: MVar String 
  }


-- A hash-table from head to list of requested resources...
type HeadsHashTable = H.BasicHashTable B.ByteString [(String, String)]

-- A hash-table from resource uri to time when it was last sent


data FsWorkerServicePocket  = FsWorkerServicePocket {
    headsHashTable      :: MVar HeadsHashTable
    ,hostPort           :: HostPort
    ,hostPortByteString :: B.ByteString
  }


-- TODO: Enrich this....
data FsWorkerParams = FsWorkerParams


instance StreamWorkerClass FsWorkerParams FsWorkerServicePocket FsWorkerSessionPocket where
 
    initService _ = do 
        heads_hash_table <- H.new
        heads_hash_table_mvar <- newMVar heads_hash_table
        host_port <- getHostPort
        host_port_bytestring <- return $ pack $ (fst host_port) ++ ":" ++(show $ snd host_port)
        return $ FsWorkerServicePocket {
            headsHashTable = heads_hash_table_mvar
            ,hostPort = host_port 
            ,hostPortByteString = host_port_bytestring
            }
 
    initSession _ = do
        session_is_new       <- newMVar True 
        session_is_recording <- newEmptyMVar
        return FsWorkerSessionPocket {
            sessionIsNew = session_is_new
            ,sessionIsRecording = session_is_recording
        }
 
    initStream r s = fsWorker r s



fsWorker :: FsWorkerServicePocket -> FsWorkerSessionPocket -> IO StreamWorker 
fsWorker service_pocket session_pocket = return $ do 
    input_token_maybe <- await 
    www_dir <- liftIO $ wwwDir
    case input_token_maybe of 
        Nothing     ->  do 
            return ()

        Just (Headers_STk headers) -> do

            case (maybe_real_path, method) of    

                (Just uu, "GET") -> do
                    liftIO $ putStrLn $ "Got relUri= " ++ the_path
                    -- Very basic security
                    if  (".." `isInfixOf` the_path ) || ("%" `isInfixOf` the_path) then 
                        send404 
                    else do
                        full_path <- return $ www_dir </> relativized_path
                        maybe_contents <- liftIO $ fetchFile full_path

                        case maybe_contents of 

                            Just contents -> do

                                -- Seems I can deliver something.... 
                                continuation <- cacheInteract the_path full_path service_pocket session_pocket
                                sendResponse the_path contents
                                continuation

                            -- TODO: Refactor and remove duplications
                            Nothing ->  do 
                                check <- liftIO $ pathGoesToIndex full_path relativized_path
                                case check of 
                                    Just index_fname -> do
                                        Just contents2 <- liftIO $ fetchFile index_fname
                                        continuation <- cacheInteract the_path index_fname service_pocket session_pocket
                                        sendResponse index_fname contents2 
                                        continuation

                                    Nothing -> send404
                  where 
                    the_path = (U.uriPath uu) ++ (U.uriQuery uu)
                    relativized_path = tail $ U.uriPath uu


                _ -> send404

          where 
            (Just path)     = getHeader headers ":path"
            str_path        = unpack path
            maybe_real_path = U.parseRelativeReference str_path
            (Just method)   = getHeader headers ":method"


type ContinuationWorker a = ConduitM StreamInputToken StreamOutputAction IO a


cacheInteract :: String
                 -> String 
                 -> FsWorkerServicePocket
                 -> FsWorkerSessionPocket
                 -> ContinuationWorker StreamWorker
cacheInteract the_path full_filename service_pocket session_pocket = do

    liftIO $ putStrLn $ "CACHE interact with " ++ the_path

    session_is_new <- liftIO $ modifyMVar (sessionIsNew session_pocket) $ 
        \ session_is_new -> do 
            return (False, session_is_new)

    head_is_known <- liftIO $ headIsKnown service_pocket the_path
    
    if session_is_new then do 

        if head_is_known then 
            pushResourcesOfHead  service_pocket the_path
        else do 
            liftIO $ do
                registerHead service_pocket the_path
                setSessionOnRecordingMode session_pocket the_path
            nothingToSend
    else do
        recording_for <- liftIO $ getRecordingState session_pocket
        case (head_is_known, recording_for) of

            (False, (Just  url_head))  -> do
                liftIO $ recordThis service_pocket url_head the_path full_filename
                nothingToSend

            (True, Nothing)            ->  
                pushResourcesOfHead service_pocket the_path


            (True, (Just _))           -> do
                -- Deactivate the  recording function 
                liftIO $ disableRecordingOnSession session_pocket
                pushResourcesOfHead service_pocket the_path

            _                        ->
                nothingToSend


nothingToSend :: ConduitM StreamInputToken StreamOutputAction IO StreamWorker
nothingToSend = return $ liftIO $ return ()

-- Continuation style for push-resources
pushResourcesOfHead :: FsWorkerServicePocket
                 -> String
                 -> ConduitM StreamInputToken StreamOutputAction IO StreamWorker
pushResourcesOfHead service_pocket the_path = do 
    pushHeadersOfHead service_pocket the_path 
    return $ pushContentsOfHead service_pocket the_path


headIsKnown :: FsWorkerServicePocket -> String -> IO Bool
headIsKnown service_pocket the_path = do 
    ht     <- readMVar $ headsHashTable service_pocket
    answer <- H.lookup ht (pack the_path)
    case answer of 
        Just _ -> return True 

        Nothing -> return False 


registerHead :: FsWorkerServicePocket
                -> String 
                -> IO ()
registerHead service_pocket head_url = do 
    -- Lock the heads hash table....
    ht     <- takeMVar ht_mvar
    -- Create a new list of resources
    putStrLn $ "Inserted: " ++ head_url 
    H.insert ht (pack head_url) []

    -- Unlock the heads hash table... 
    putMVar ht_mvar ht
  where 
    ht_mvar = headsHashTable service_pocket


pushHeadersOfHead ::  FsWorkerServicePocket
                 -> String -> ConduitM StreamInputToken StreamOutputAction IO ()
pushHeadersOfHead service_pocket the_path = do 

    urls_and_filepaths <- liftIO $ do 
        ht     <- readMVar ht_mvar
        Just ( urls_and_filepaths ) <- H.lookup ht (pack the_path)
        return urls_and_filepaths

    -- And send associated files, marking them as such.... 
    mapM_ (\ (i, (a_url, a_filepath)) -> do
            -- TODO: I'm reading off the resource just to have its length ...
            -- I seriously need to consider using an in-memory cache.
            Just contents <- liftIO $ fetchFile a_filepath 
            sendAssociatedHeaders i a_url host_port_bytestring contents
        ) (enum urls_and_filepaths)
  where 
    ht_mvar = headsHashTable service_pocket
    enum x = zip [0 .. ] x
    host_port_bytestring = hostPortByteString service_pocket


pushContentsOfHead ::  FsWorkerServicePocket
                 -> String -> ConduitM StreamInputToken StreamOutputAction IO ()
pushContentsOfHead service_pocket the_path = do 

    urls_and_filepaths <- liftIO $ do 
        ht     <- readMVar ht_mvar
        Just ( urls_and_filepaths ) <- H.lookup ht (pack the_path)
        return urls_and_filepaths

    -- And send associated files, marking them as such.... 
    mapM_ (\ (i, (_, a_filepath)) -> do 
            Just contents <- liftIO $ fetchFile a_filepath
            sendAssociatedData i contents
        ) (enum urls_and_filepaths)
  where 
    ht_mvar = headsHashTable service_pocket
    enum x = zip [0 .. ] x


setSessionOnRecordingMode  :: FsWorkerSessionPocket -> String -> IO ()
setSessionOnRecordingMode session_pocket the_path = do 
    could_set <- tryPutMVar (sessionIsRecording session_pocket) the_path
    if not could_set then 
        putStrLn "Could not activate recording on session (weird)"
    else
        putStrLn "Set session on recording mode"


disableRecordingOnSession :: FsWorkerSessionPocket -> IO ()
disableRecordingOnSession session_pocket = do 
    maybe_could_take <- tryTakeMVar (sessionIsRecording session_pocket)
    case maybe_could_take of 
        Just _ -> putStrLn "Disabled recording!"
        _      -> putStrLn "COULD NOT!! disable session recording"



getRecordingState :: FsWorkerSessionPocket -> IO (Maybe String)
getRecordingState session_pocket = tryReadMVar (sessionIsRecording session_pocket)


recordThis :: FsWorkerServicePocket
                 -> String
                 -> String
                 -> String 
                 -> IO ()
recordThis service_pocket  head_path this_path full_filepath = do
    ht              <- takeMVar ht_mvar
    (Just the_list) <- H.lookup ht head_path_bs
    new_list        <- return $ (this_path, full_filepath):the_list
    H.insert ht head_path_bs new_list 
    putMVar  ht_mvar ht
  where 
    ht_mvar = headsHashTable service_pocket
    head_path_bs = pack head_path


sendResponse :: String -> B.ByteString -> StreamWorker
sendResponse the_path contents = do 
    mimetype <- return $ getRelPathMime the_path
    yield $ SendHeaders_SOA $ UnpackedNameValueList  [
         (":status", "200")
        ,(":version", "HTTP/1.1")
        ,("content-length", (pack.show $ B.length contents))
        ,("content-type",  mimetype)

        -- TODO here: set the no-cache headers .... 

        ,("server", "ReH v0.0")
        ]
    yield $ SendData_SOA contents
    yield $ Finish_SOA

-- subid: a local stream id. The output plug is in charge of translating
-- this to an actual stream id. This local stream id is needed to match 
-- headers and body data
sendAssociatedHeaders :: Int -> String ->  B.ByteString -> B.ByteString  -> StreamWorker
sendAssociatedHeaders subid the_path host_port contents = do 
    mimetype <- return $ getRelPathMime the_path
    yield $ SendAssociatedHeaders_SOA subid $ UnpackedNameValueList  [
         (":status", "200")
        ,(":version", "HTTP/1.1")
        ,(":scheme", "https")
        ,(":host",  host_port )
        ,(":path", pack the_path)
        ,("acquisition-intent", "push")
        ,("content-length", (pack.show $ B.length contents))
        ,("content-type",  mimetype)


        -- TODO here: set the no-cache headers .... 

        ,("server", "ReH v0.0")
        ]


sendAssociatedData :: Int -> B.ByteString -> StreamWorker
sendAssociatedData  subid contents = do
    yield $ SendAssociatedData_SOA subid contents
    yield $ SendAssociatedFinish_SOA subid


pathGoesToIndex :: String -> String -> IO (Maybe String)
pathGoesToIndex abs_path relpath = do 
    if perhaps then do
        b <- doesFileExist index_html
        if b then 
            return $ Just index_html 
        else
            return Nothing
    else
        return Nothing
  where
    perhaps = "/" `isSuffixOf` abs_path || relpath == ""
    index_html = if relpath == "" then
        abs_path ++ "/index.html"
    else 
        abs_path ++ "index.html"


fetchFile :: String ->  IO (Maybe B.ByteString)
fetchFile full_path  = do
    exists <- doesFileExist full_path
    if exists then
      do 
        contents <- B.readFile full_path
        return $ Just contents
    else 
      do 
        return $ Nothing
 

bad404ResponseData :: B.ByteString 
bad404ResponseData = "404: ReH: Didn't find that"


bad404ResponseHeaders ::  UnpackedNameValueList 
bad404ResponseHeaders =  UnpackedNameValueList [
                 (":status", "404")
                ,(":version", "HTTP/1.1")
                ,("content-length", (pack.show $ B.length bad404ResponseData))
                ,("content-type", "text/plain")
                ,("server", "ReHv0.0")
                ]


suffixToMimeTypes :: [ (B.ByteString, B.ByteString) ]
suffixToMimeTypes = [
    (".js", "application/x-javascript")
    ,(".html", "text/html")
    ,(".css", "text/css")
    ,(".svg", "image/svg+xml")
    ,(".json", "application/json")
    ,(".txt", "text/plain")
    ]


getRelPathMime :: String -> B.ByteString
getRelPathMime  rel_path = case maybe_mime of 
    Just (_, mime_type) -> mime_type
    Nothing             -> 
        case maybe_mime2 of 
            Just (_, mime_type)    -> mime_type 
            Nothing                -> "application/octet-stream" 
  where 
    maybe_mime  = find (\ (ext, _) -> ext `B.isSuffixOf` rel_path_bs ) suffixToMimeTypes
    maybe_mime2 = find (\ (ext, _) -> (B.append ext  "?") `B.isInfixOf` rel_path_bs ) suffixToMimeTypes
    rel_path_bs = pack rel_path


send404 :: ConduitM StreamInputToken StreamOutputAction IO ()
send404 = do 
    yield $ SendHeaders_SOA bad404ResponseHeaders
    yield $ SendData_SOA bad404ResponseData
    yield $ Finish_SOA

