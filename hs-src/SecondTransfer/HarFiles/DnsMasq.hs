{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module SecondTransfer.HarFiles.DnsMasq (
    dnsMasqFileContents
    ,dnsMasqFileContentsToIp
    ) where 


import qualified Data.ByteString as B 

dnsMasqFileContents :: [B.ByteString] -> B.ByteString
dnsMasqFileContents all_seen_hosts = B.intercalate "\n" $ map 
    (\ hostname -> B.append "127.0.0.1      " hostname) 
    all_seen_hosts


dnsMasqFileContentsToIp :: B.ByteString -> [B.ByteString] -> B.ByteString
dnsMasqFileContentsToIp ip all_seen_hosts = B.intercalate "\n" $ map 
    (\ hostname -> B.concat [ip, "      ", hostname]) 
    all_seen_hosts