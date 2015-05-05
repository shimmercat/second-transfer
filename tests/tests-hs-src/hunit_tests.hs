import Test.HUnit

import SecondTransfer.Test.DecoySession

import Tests.HTTP2Session
import Tests.Utils


tests = TestList [
	TestLabel "testPrefaceChecks" testPrefaceChecks,
	TestLabel "testPrefaceChecks2" testPrefaceChecks2,
    TestLabel "testLowercaseHeaders" testLowercaseHeaders
	]


main = do 
	runTestTT tests 
	return ()