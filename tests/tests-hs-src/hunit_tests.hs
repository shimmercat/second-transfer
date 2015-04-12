import Test.HUnit

import SecondTransfer.Test.DecoySession

import Tests.HTTP2Session


tests = TestList [
	TestLabel "testPrefaceChecks" testPrefaceChecks,
	TestLabel "testPrefaceChecks2" testPrefaceChecks2
	]


main = do 
	runTestTT tests 
	return ()