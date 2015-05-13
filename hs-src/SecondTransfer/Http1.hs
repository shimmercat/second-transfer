{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module SecondTransfer.Http1(
    newIncrementalHttp1Parser

    ,IncrementalHttp1Parser
    ) where 

import Control.Lens
import Data.Word(Word8)
import qualified Control.Lens as L

import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as Lb
import qualified Data.ByteString.Builder as Bu
import           Data.Monoid   (mappend, mconcat, mempty)


data IncrementalHttp1Parser = IncrementalHttp1Parser {
    _fullText :: Bu.Builder
    ,stateParser :: InternalState
    }

data InternalState = 
     ParsingHeaders (B.ByteString ->  ([Int], Int, Word8))
    |ParsingBody 

L.makeLenses ''IncrementalHttp1Parser

newIncrementalHttp1Parser :: IncrementalHttp1Parser
newIncrementalHttp1Parser = IncrementalHttp1Parser {
    _fullText = mempty
    ,_headParser = locateCRLFs 0 [] 0
    }


-- | Was the parser complete?
data Http1ParserCompletion = 
    -- ^ No, not even headers are done. Use the returned
    --   value to continue
     MustContinue_H1PC IncrementalHttp1Parser
    -- ^ Headers were completed. For some methods that's all
    --   there is, so the next-in-line is either a left-overs
    --   string (not an error one!), or if a body should be present,
    --   a new IncrementalHttp1Parser
    |Headers_H1PC      Http1Head (Either B.ByteString IncrementalHttp1Parser)
    -- ^ For requests with a body. The second argument is the body,
    --   which in the general case is a composition of chunks
    |Done_H1PC         Http1Head Lb.ByteString B.ByteString

    
addBytes :: IncrementalHttp1Parser -> B.ByteString -> Http1ParserCompletion
addBytes = error "Notdone"


locateCRLFs :: Int -> [Int] -> Word8 ->  B.ByteString ->  ([Int], Int, Word8)
locateCRLFs initial_offset other_positions prev_last_char next_chunk =
  let 
    (last_char, positions_list, strlen) =
        B.foldl 
            (\ (prev_char, lst, i) w8 -> 
                let 
                    ip1 = i + 1
                in case (prev_char, w8) of 
                    (13,10) -> (w8, i:lst, ip1)
                    _       -> (w8, lst, ip1)
            )
            (prev_last_char, other_positions, initial_offset)
  in (positions_list, strlen, last_char)
