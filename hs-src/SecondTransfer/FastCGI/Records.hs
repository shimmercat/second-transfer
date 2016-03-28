{-# LANGUAGE TemplateHaskell, OverloadedStrings,  DeriveGeneric, RankNTypes #-}
module SecondTransfer.FastCGI.Records (
                writeOneOrFourLength
              , RecordType                              (..)
              , putBeginRequest
              , BeginRequest_Rec                        (..)

              , wrapRecordFrame
              , toWrappedStream

              , writeParameterPair
    ) where


import           Data.Binary
--import           Data.Word
import           Data.Int
import           Data.Bits
import           Data.Binary.Put

import qualified Data.ByteString                        as B
import qualified Data.ByteString.Lazy                   as LB
import qualified Data.ByteString.Builder                as Bu

import qualified Data.Attoparsec.ByteString             as ATO
--import qualified Data.Attoparsec.ByteString.Char8       as ATO8

import           Control.Monad
import           Data.Conduit


data RecordType =
     Zero_RT
   | BeginRequest_RT
   | AbortRequest_RT
   | EndRequest_RT
   | Params_RT
   | Stdin_RT
   | Stdout_RT
   | Stderr_RT
   | Data_RT
   | GetValues_RT
   | GetValuesResult_RT
    deriving (Show, Eq, Enum, Ord)


data FastCGIRoles =
    Responder_fcgiR
  | Authorizer_fcgiR
  | Filter_fcgiR
    deriving (Show, Eq, Enum, Ord)


data BeginRequest_Rec = BeginRequest_Rec {
    _applicationClosesConnection_BR :: Bool
    }
    deriving Show


putBeginRequest :: BeginRequest_Rec -> Put
putBeginRequest (BeginRequest_Rec { _applicationClosesConnection_BR = p1 }) =
  do
    -- Put responder role
    put (1 :: Word16 )
    -- Put if the application closes the connection
    if p1
      then putWord8 0
      else putWord8 1

    -- The reserved Bytes ..
    forM_ [1..5::Integer] (const $ putWord8 0)


readOneOrFourLength :: ATO.Parser Int
readOneOrFourLength =
  do
    aw8 <- ATO.anyWord8
    if aw8 .|. 128 == 0
      then
          return $ fromIntegral aw8
      else do
          b2 <- fromIntegral <$> ATO.anyWord8
          b1 <- fromIntegral <$> ATO.anyWord8
          b0 <- fromIntegral <$> ATO.anyWord8
          return
              $ (fromIntegral $ aw8 .&. 127 ) `shiftL` 24 +
                b2 `shiftL` 16 +
                b1 `shiftL` 8 +
                b0


writeOneOrFourLength :: Int -> Bu.Builder
writeOneOrFourLength n | n < 128  = Bu.word8 . fromIntegral $ n
                       | otherwise =
                         mconcat [
                         Bu.word8 . fromIntegral $
                             ( (n `shiftR` 24 ) .|. 128  ) ,
                         Bu.word8 . fromIntegral $
                             ( (n `shiftR` 16 ) .&. 255  ) ,
                         Bu.word8 . fromIntegral $
                             ( (n `shiftR` 8 ) .&. 255  ) ,                                           Bu.word8 . fromIntegral $
                             ( n .&. 255  ) ]




readParameterPair :: ATO.Parser (B.ByteString, B.ByteString)
readParameterPair =
  do
    name_length <- readOneOrFourLength
    value_length <- readOneOrFourLength
    param_name <- ATO.take name_length
    param_value <- ATO.take value_length
    return (param_name, param_value)


writeParameterPair :: (B.ByteString, B.ByteString) -> Bu.Builder
writeParameterPair (hn, hv) =
  let

    bu_hn = Bu.byteString hn
    bu_hv = Bu.byteString hv

    lng_hn = writeOneOrFourLength $ B.length hn
    lng_hv = writeOneOrFourLength $ B.length hv
  in
    mconcat [
        lng_hn,
        lng_hv,
        bu_hn,
        bu_hv
        ]


data RecordFrame = RecordFrame {
    _type_RH          :: RecordType
  , _requestId_RH     :: Int
  , _payload_RH       :: B.ByteString
    }
    deriving (Show)


readRecordFrame :: ATO.Parser RecordFrame
readRecordFrame =
  do
    _ <- ATO.word8 1 -- The reserved
    packet_type <- (toEnum . fromIntegral) <$> ATO.anyWord8
    request_id_b1 <- ATO.anyWord8
    request_id_b0 <- ATO.anyWord8
    let
        request_id = fromIntegral request_id_b0 + (256 :: Int) * fromIntegral request_id_b1
    content_length_b1 <- ATO.anyWord8
    content_length_b0 <- ATO.anyWord8
    let
        content_length = fromIntegral content_length_b0 + (256 :: Int) * (fromIntegral content_length_b1)
    padding_length <- ATO.anyWord8
    _reserved <- ATO.anyWord8
    content_data <- ATO.take content_length
    _padding_data <- ATO.take (fromIntegral padding_length)

    return RecordFrame {
        _type_RH = packet_type
      , _requestId_RH = request_id
      , _payload_RH = content_data
        }


wrapRecordFrame :: RecordType -> Int -> LB.ByteString -> Put
wrapRecordFrame record_type request_id payload =
  do
    putWord8 1 -- The version number
    putWord8 . fromIntegral . fromEnum $ record_type
    let
        request_id_b1 = fromIntegral $  request_id `div` 256
        request_id_b0 = fromIntegral $  request_id `mod` 256
        content_length :: Word16
        content_length = fromIntegral $ LB.length payload

        content_length_b1 = fromIntegral $  content_length `div` 256
        content_length_b0 = fromIntegral $ content_length `mod` 256

    putWord8 request_id_b1
    putWord8 request_id_b0

    putWord8 content_length_b1
    putWord8 content_length_b0

    putWord8 0 -- Padding length

    -- Now finally put the data
    putLazyByteString payload

    -- And we are done

maxChunkLength :: Int64
maxChunkLength = 32000

toWrappedStream ::
  forall m . Monad m => RecordType -> Int -> Conduit LB.ByteString m LB.ByteString
toWrappedStream record_type request_id =
  let

    -- go :: LB.ByteString -> Conduit LB.ByteString m LB.ByteString
    go leftovers
      | LB.length leftovers > maxChunkLength = do
          let
              (send_now, send_later) = LB.splitAt maxChunkLength leftovers
              wrapped_frame = runPut $
                  wrapRecordFrame record_type request_id send_now
          yield wrapped_frame
          go send_later
      | LB.length leftovers > 0 = do
          let
              wrapped_frame = runPut $
                  wrapRecordFrame record_type request_id leftovers
          yield wrapped_frame
          go ""
      | otherwise = do
          maybe_new_data <- await
          case maybe_new_data of
              Nothing -> return ()
              Just fragment -> go fragment
  in
    go ""
