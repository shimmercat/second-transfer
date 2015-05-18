{-# LANGUAGE OverloadedStrings #-}
module Tests.Utils where


import qualified Data.ByteString                  as B

import           Test.HUnit

import           SecondTransfer                   (Headers)
import           SecondTransfer.Utils.HTTPHeaders 
import           SecondTransfer.Http1.Internal    (locateCRLFs)


testLowercaseHeaders :: Test
testLowercaseHeaders = TestCase $ do 
    let 
        irregular = [
            ("A", "B"),
            ("C-D", "D")
            ] :: Headers
        regular = lowercaseHeaders irregular
    assertEqual "lowercase" regular [
        ("a", "B"),
        ("c-d", "D")
        ]
    assertEqual "test-lowercase-1" (headersAreValidHTTP2 irregular) False
    assertEqual "test-lowercase-2" (headersAreValidHTTP2 regular) True


testCRLFLocate :: Test 
testCRLFLocate = TestCase $ do 
    let 
        input_text = "hello\r\nworld"
        (positions, length_, lastchar) = locateCRLFs 0 [] 0 input_text
    assertEqual "position-is-5.position" (head positions) 5
    assertEqual "position-is-5.length" length_ (B.length input_text)
    assertEqual "position-is-5.lastchar" lastchar (fromIntegral . fromEnum $ 'd')


    let 
        input_text_2 = "hello\r\nworld\r\n\r\n"
        (positions, length_, lastchar) = locateCRLFs 0 [] 0 input_text_2
    assertEqual "many-positions.position" positions [14,12,5]
    assertEqual "many-positions.length" length_ (B.length input_text_2)
    assertEqual "many-positions.lastchar" lastchar (fromIntegral . fromEnum $ '\n')


testReplaceHostByAuthority :: Test 
testReplaceHostByAuthority = TestCase $ do 
    let 
        headers_pre = [
            ("A", "B"),
            ("C-D", "D"),
            ("Host", "D")
            ] :: Headers
        regular = toList. replaceHostByAuthority . fromList . lowercaseHeaders $ headers_pre
    -- Do not pass in headers with uppercase
    assertEqual "replace-host-by-authority" 
        [
            (":authority", "D"),
            ("a", "B"),
            ("c-d", "D")
        ]
        regular