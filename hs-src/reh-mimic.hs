
{-# LANGUAGE OverloadedStrings #-}


-- System grade imports
-- import qualified Data.ByteString            as B
-- import qualified Data.ByteString.Builder    as Bu
-- import           Data.ByteString.Char8      (pack)
-- import qualified Data.ByteString.Lazy       as BL
import           Data.Foldable             (find)
import           Data.Monoid
import           Text.Printf               (printf)

-- import           System.Directory
-- import           System.FilePath
import           Control.Exception         (catch)
--
--
import           System.Environment        (getEnvironment)
import           System.IO
-- import           System.Process

import           Options.Applicative

-- Logging utilities
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog (Facility (..), Option (..), openlog)
import           System.Log.Logger

-- Imports from other parts of the program

import           SecondTransfer.MainLoop.ConfigHelp  (mimicDataDir)
import           SecondTransfer.Research.Main        (research)
import           SecondTransfer.MainLoop.OpenSSL_TLS (ConnectionIOError(..))



-- What is the program going to do?
data ProgramAction = 
    ResearchUrl_PA -- Wait for a POST request from the browser


actionStrToAction :: String -> ProgramAction
actionStrToAction "research"     = ResearchUrl_PA
actionStrToAction _              = error "Action doesn't exist"


data Program  = Program {
    action   :: ProgramAction
    -- ,harFileName :: String
    }


programParser :: Parser Program
programParser = Program <$> (
    actionStrToAction <$>
        strOption
             ( long "action"
                <> metavar "ACTION"
                <> help    "What's the program going to do: only \"research\" is supported now." )
        )    


main :: IO ()
main = do
    env_vars <- getEnvironment
    case find (\ (name, _) -> name == "MIMIC_CONSOLE_LOGGER") env_vars of 

        Nothing                                         -> configureLoggingToSyslog 

        Just (_, x) | not $ (length x) == 0 || x == "0" -> configureLoggingToConsole

                    | otherwise                         -> configureLoggingToSyslog


    mimic_dir <- mimicDataDir
    infoM "RehMimic" $ printf "Mimic dir: \"%s\"" mimic_dir
    prg   <-  execParser opts_metadata

    catch 
        (case Main.action prg of 

            ResearchUrl_PA       -> do 
                research mimic_dir
        )
        (\ (ConnectionIOError msg) -> errorM "HTTP2.Session" ("ConnectionIOError: " ++ msg)
        )


  where 
    opts_metadata = info 
        ( helper <*> programParser )
        ( fullDesc 
            <>  progDesc 
                ( "Mimics web servers. Use environmnet variables MIMIC_DATA_DIR to point to where the scratch directory" ++
                  "of the server is. Use the variable MIMIC_CONSOLE_LOGGER with value 1 to log messages to console; otherwise " ++
                  "they are going to syslog. " )

            <>  header   "reh-mimic"
            )


configureLoggingToConsole :: IO ()
configureLoggingToConsole = do 
    s <- streamHandler stderr DEBUG  >>= 
        \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger "HTTP2.Session" (
        setHandlers [s] .  -- Remember that composition works in reverse...
        setLevel ERROR  
        )
    updateGlobalLogger "OpenSSL" (
        setHandlers [s] .  -- Remember that composition works in reverse...
        setLevel INFO  
        )
    updateGlobalLogger "HarWorker" (
        setHandlers [s] .  -- Remember that composition works in reverse...
        setLevel DEBUG  
        )
    updateGlobalLogger "ResearchWorker" (
        setHandlers [s] .  -- Remember that composition works in reverse...
        setLevel DEBUG  
        )


configureLoggingToSyslog :: IO ()
configureLoggingToSyslog = do 
    s <- openlog "RehMimic" [PID] DAEMON INFO >>= 
        \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger "HTTP2.Session" (
        setHandlers [s] .  -- Remember that composition works in reverse...
        setLevel ERROR  
        )
    updateGlobalLogger "OpenSSL" (
        setHandlers [s] .
        setLevel INFO  
        )
    updateGlobalLogger "HarWorker" (
        setHandlers [s] .
        setLevel DEBUG  
        )
    updateGlobalLogger "ResearchWorker" (
        setHandlers [s] .
        setLevel DEBUG  
        )