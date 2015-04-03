{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}

module SecondTransfer.MainLoop.ConfigHelp(
	configDir
	,mimicDataDir
	,wwwDir
	,getServedHost
	,getServedPort
	,getHostPort
	,getInterfaceName
	,getMimicPort
	,getPrivkeyFilename
	,getCertFilename
	,getMimicPostPort
	,getMimicPostInterface

	,HostPort
	) where 


import           Control.Exception

import           System.Directory
import           System.Environment
import           System.FilePath


import           Data.Typeable
import           Data.Aeson.Types      ()
import qualified Data.ByteString       as B

import           SecondTransfer.Utils            (stripString)


data BadAesonFile = BadAesonFile B.ByteString
    deriving (Show, Typeable)

instance Exception BadAesonFile

type HostPort = (String, Int)


kelDataDir :: IO FilePath
kelDataDir = 
	catch 
		(getEnv "KEL_DATA_DIR")
		noEnvErrorHandler


mimicDataDir :: IO FilePath
mimicDataDir = 
	catch 
		(getEnv "MIMIC_DATA_DIR")
		noEnvErrorHandler


configDir :: FilePath -> FilePath 
configDir base_dir = do 
	base_dir </> "config"


wwwDir :: IO FilePath
wwwDir = do 
	data_dir <- kelDataDir 
	return $ data_dir </> "www"


noEnvErrorHandler :: IOError -> IO FilePath 
noEnvErrorHandler _ = do 
	putStrLn "NoEnvironment"
	getCurrentDirectory


getServedHost :: FilePath -> IO String 
getServedHost config_dir = do 
	contents <- readFile $ config_dir </> "servedhost.conf"
	return $ contents


getCertFilename :: FilePath -> FilePath 
getCertFilename config_dir = config_dir </> "servercert.pem"


getPrivkeyFilename :: FilePath -> FilePath 
getPrivkeyFilename config_dir = config_dir </> "privkey.pem"


getInterfaceName :: FilePath -> IO String 
getInterfaceName config_dir = do 
	contents <- readFile $ config_dir </> "useinterface.conf"
	return $ contents


getServedPort :: FilePath -> IO Int 
getServedPort config_dir = do 
	contents <- readFile $ config_dir </> "port.conf"
	return $ read contents


getHostPort :: FilePath -> IO HostPort
getHostPort config_dir = do 
	served_host <- getServedHost config_dir
	served_port <- getServedPort config_dir

	return (served_host, served_port)


getMimicPort :: IO Int
getMimicPort = do 
	mimic_dir <- mimicDataDir
	let config_dir = configDir mimic_dir
	contents <- readFile $ config_dir </> "port.conf"
	return $ read contents


getMimicPostPort :: FilePath -> IO Int
getMimicPostPort config_dir = do 
	contents <- readFile $ config_dir </> "post-port.conf"
	return $  read contents


getMimicPostInterface :: FilePath -> IO String
getMimicPostInterface config_dir = do 
	contents <-  readFile $ config_dir </> "post-interface.conf"
	return $ stripString contents

