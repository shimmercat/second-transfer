{-# LANGUAGE OverloadedStrings #-}
module Tests.TestIOCalbks where

import           Control.Lens

import qualified Data.ByteString                     as B
import qualified Data.ByteString.Lazy                as LB
import qualified Data.ByteString.Builder             as Bu
import           Data.Maybe                          (isJust)

import           Test.HUnit

import           SecondTransfer.IOCallbacks.Types
import           SecondTransfer.IOCallbacks.Coupling

import           SecondTransfer                      (Headers)
import           SecondTransfer.Http1.Parse
import qualified SecondTransfer.Utils.HTTPHeaders    as He


testPair :: Test
testPair = TestCase $ do
  (ioap, iobp) <- popIOCallbacksIntoExistance
  ioa <- handshake ioap
  iob <- handshake iobp
  (ioa ^. pushAction_IOC) "Hello world"
  n <- (iob ^. pullAction_IOC) $ B.length "Hello world"
  assertEqual "testTransitsInPair" n "Hello world"


testPair2 :: Test
testPair2 = TestCase $ do
  (ioap, iobp) <- popIOCallbacksIntoExistance
  ioa <- handshake ioap
  iob <- handshake iobp
  (ioa ^. pushAction_IOC) "Hello world"
  n <- (iob ^. pullAction_IOC) $ B.length "Hello world" - 1
  assertEqual "testTransitsInPair" n "Hello worl"
  (ioa ^. pushAction_IOC) "xebo"

  nn <- (iob ^. pullAction_IOC) $ B.length "dxebo"
  assertEqual "testTransitsInPair2" nn "dxebo"


testPair3 :: Test
testPair3 = TestCase $ do
  (ioap, iobp) <- popIOCallbacksIntoExistance
  ioa <- handshake ioap
  iob <- handshake iobp
  (ioa ^. pushAction_IOC) "Hello world"
  n <- (iob ^. pullAction_IOC) $ B.length "Hello world" - 1
  assertEqual "testTransitsInPair" n "Hello worl"
  nn <- (iob ^. bestEffortPullAction_IOC) False
  assertBool "testTransitsInPair2" (B.length nn > 0)
