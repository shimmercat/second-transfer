{-# LANGUAGE
     OverloadedStrings,
     TemplateHaskell,
     FunctionalDependencies,
     FlexibleContexts,
     Rank2Types #-}
module SecondTransfer.FastCGI.Probe (
                 probe
    ) where


import           Control.Lens
import qualified Control.Exception                                    as E
import           Control.Monad.IO.Class                               (liftIO)
import           Control.Concurrent                                   hiding (yield)

--import qualified Data.ByteString                                      as B
import qualified Data.ByteString.Lazy                                 as LB
import qualified Data.ByteString.Builder                              as Bu
--import qualified Data.Binary                                          as Bin
import qualified Data.Binary.Put                                      as Bin
import           Data.Conduit
import qualified Data.Conduit.List                                    as CL

import           SecondTransfer.Exception
import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.Sessions.ApplicationPortProbe

import           SecondTransfer.FastCGI.Records
import           SecondTransfer.FastCGI.GenericSession                (framesParser)
--import qualified Data.Conduit.Attoparsec                              as DCA


probe :: IOCallbacks -> IO ProbeResult
probe ioc =
  do
    -- We assume the transport is already open, let's just send  a special packet
    let
        bu =
            writeParameterPair
                ("FCGI_MAX_CONNS",
                "")

        open_packet = wrapRecordFrame
            GetValues_RT
            0
            (Bu.toLazyByteString bu)

        bepa = (ioc ^. bestEffortPullAction_IOC) True

        source = do
            b <- liftIO bepa
            CL.sourceList $ LB.toChunks b
            source

    packet_sent <- E.try $
        (ioc ^. pushAction_IOC) (Bin.runPut open_packet)
    --putStrLn "Probe packet sent"

    case packet_sent :: Either IOProblem () of
        Left e -> return . Left $ E.displayException e
        Right _ -> do
            -- Next step it is to see if I can get an answer from the application, without
            -- the application crashing in a million fragments...
            -- TODO: Is it any danger of time-out here?
            -- Yes, there is danger of time-out
            --putStrLn "Waiting for response"

            _ <- forkIOExc "probeTimeOutExc" $ do
                threadDelay (1000*1000)
                (ioc ^. closeAction_IOC)

            either_response_frame <- E.try . runConduit $
                source
                =$=
                framesParser
                =$=
                CL.take 1
            --putStrLn "Response received"

            case either_response_frame :: Either E.SomeException [RecordFrame] of
                Left e -> return . Left $ E.displayException e
                Right _ -> return . Right $  ()
