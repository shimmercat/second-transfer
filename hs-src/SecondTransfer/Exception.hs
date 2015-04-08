{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
module SecondTransfer.Exception (
	-- * Exceptions thrown by the HTTP/2 sessions
	HTTP2SessionException (..)
	,FramerException (..)
	,BadPrefaceException (..)
	) where 

import           Control.Exception
import           Data.Typeable


-- | Abstract exception. All HTTP/2 exceptions derive from here 
data HTTP2SessionException = forall e . Exception e => HTTP2SessionException e
    deriving Typeable

instance Show HTTP2SessionException where
    show (HTTP2SessionException e) = show e

instance Exception HTTP2SessionException 

convertHTTP2SessionExceptionToException :: Exception e => e -> SomeException
convertHTTP2SessionExceptionToException = toException . HTTP2SessionException

getHTTP2SessionExceptionFromException :: Exception e => SomeException -> Maybe e
getHTTP2SessionExceptionFromException x = do
    HTTP2SessionException a <- fromException x
    cast a



-- | Abstract exception. Thrown when encoding/decoding of a frame fails
data FramerException = forall e . Exception e => FramerException e
    deriving Typeable

instance Show FramerException where
    show (FramerException e) = show e

instance Exception FramerException where
    toException = convertHTTP2SessionExceptionToException
    fromException = getHTTP2SessionExceptionFromException

convertFramerExceptionToException :: Exception e => e -> SomeException
convertFramerExceptionToException = toException . FramerException

getFramerExceptionFromException :: Exception e => SomeException -> Maybe e
getFramerExceptionFromException x = do
    FramerException a <- fromException x
    cast a



-- | Thrown when the HTTP/2 connection prefix doesn't 
--   match the expected prefix.
data BadPrefaceException = BadPrefaceException
    deriving (Typeable, Show)

instance Exception BadPrefaceException where
    toException   = convertFramerExceptionToException
    fromException = getFramerExceptionFromException