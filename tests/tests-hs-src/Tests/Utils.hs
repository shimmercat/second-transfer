{-# LANGUAGE OverloadedStrings #-}
module Tests.Utils where

import Test.HUnit    

import SecondTransfer.Utils.HTTPHeaders   (lowercaseHeaders, headersAreValidHTTP2)
import SecondTransfer                     (Headers)


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