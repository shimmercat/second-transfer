{-# LANGUAGE OverloadedStrings #-}
module Tests.Utils where

import Test.HUnit    

import SecondTransfer.Utils.HTTPHeaders   (lowercaseHeaders)
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
        ("c", "D")
        ]