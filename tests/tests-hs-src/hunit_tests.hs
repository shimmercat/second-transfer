
module Main where 

import System.Exit

import Test.HUnit

import SecondTransfer.Test.DecoySession
import SecondTransfer

import Tests.HTTP2Session
import Tests.Utils
import Tests.HTTP1Parse
import Tests.TestIOCalbks


tests = TestList [
    --TestLabel "testPrefaceChecks" testPrefaceChecks
    TestLabel "testPrefaceChecks2" testPrefaceChecks2
    ,TestLabel "testLowercaseHeaders" testLowercaseHeaders
    ,TestLabel "testCRLFLocate" testCRLFLocate
    ,TestLabel "testHTTP1Parse" testParse
    ,TestLabel "testGenerate" testGenerate
    ,TestLabel "testReplaceHostByAuthority" testReplaceHostByAuthority
    ,TestLabel "testFirstFrameIsSettings" testFirstFrameMustBeSettings
    ,TestLabel "testFirstFrameIsSettings2" testFirstFrameMustBeSettings2
    -- ,TestLabel "testFirstFrameIsSettings3" testFirstFrameMustBeSettings3
    ,TestLabel "testIGet500Status" testIGet500Status
    -- ,TestLabel "testSessionBreaksOnLateError" testSessionBreaksOnLateError
    --,TestLabel "WindowUpdate to unexistent stream" testUpdateWindowFrameAborts
    ,TestLabel "testClosedInteraction0" testClosedInteraction0
    -- ,TestLabel "testClosedInteraction1" testClosedInteraction1
    ,TestLabel "testClosedInteraction3" testClosedInteraction3
    --,TestLabel "testWorkerClosesAfter"  testWorkerClosesAfter
    ,TestLabel "testWorkerClosesBefore" testWorkerClosesBefore
    ,TestLabel "testCombineAuthorityAndHost" testCombineAuthorityAndHost
    ,TestLabel "testCombineAuthorityAndHost2" testCombineAuthorityAndHost2
    ,TestLabel "testHeadersToRequest" testHeadersToRequest
    ,TestLabel "testHeadersToRequest2" testHeadersToRequest2

    ,TestLabel "testPair" testPair
    ,TestLabel "testPair2" testPair2
    ,TestLabel "testPair3" testPair3

    ,TestLabel "http1Cycle" testCycle
    ]


main = do
    rets <- runTestTT tests
    if (errors rets == 0 && failures rets == 0)
        then exitWith ExitSuccess
        else exitWith $ ExitFailure (-1)
