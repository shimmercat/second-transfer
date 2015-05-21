
module Main where 

import System.Exit

import Test.HUnit

import SecondTransfer.Test.DecoySession

import Tests.HTTP2Session
import Tests.Utils
import Tests.HTTP1Parse


tests = TestList [
	TestLabel "testPrefaceChecks" testPrefaceChecks,
	TestLabel "testPrefaceChecks2" testPrefaceChecks2,
    TestLabel "testLowercaseHeaders" testLowercaseHeaders,
    TestLabel "testCRLFLocate" testCRLFLocate,
    TestLabel "testHTTP1Parse" testParse,
    TestLabel "testGenerate" testGenerate,
    TestLabel "testReplaceHostByAuthority" testReplaceHostByAuthority
	]


main = do 
    rets <- runTestTT tests 
    if (errors rets == 0 && failures rets == 0)
        then exitWith ExitSuccess
        else exitWith $ ExitFailure (-1)
