{-# LANGUAGE OverloadedStrings #-}
module Tests.HTTP1Parse where


import qualified Data.ByteString                  as B
import           Data.Maybe                       (isJust)

import           Test.HUnit

import           SecondTransfer                   (Headers)
import           SecondTransfer.Http1.Internal    


testParse :: Test
testParse = TestCase $ do 
    let 
        headers_text                   = "GET /helo.html HTTP/1.1\r\nHost: www.auther.com \r\n\r\n"
        headers_text2                   = "POST /helo.html HTTP/1.1\r\nHost: www.auther.com \r\nContent-Length: 1\r\n\r\np"
        a0                             = newIncrementalHttp1Parser 
        isDone (OnlyHeaders_H1PC _ _)  = True 
        isDone _                       = False
        a1                             = addBytes a0 headers_text
        (OnlyHeaders_H1PC h0 leftovers)= a1 
        (HeadersAndBody_H1PC h1 cond0 l2) = addBytes a0 headers_text2
        waitForBodyOk (HeadersAndBody_H1PC _ _ _)   = True 
        waitForBodyOk _ = False 
    assertBool "testParse.IsDone" (isDone a1)
    assertEqual "testParse.NoLeftovers" leftovers ""
    assertEqual "testParse.YesLeftovers" l2 "p"
    assertEqual "testParse.FinishWellSeen" (UseBodyLength_BSC 1) cond0
