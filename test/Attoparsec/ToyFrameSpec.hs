{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Attoparsec.ToyFrameSpec
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Attoparsec.ToyFrameSpec (spec) where

import Attoparsec.ToyFrame
import Control.Exception (ArithException (..), throwIO)
import Data.Attoparsec.Frames
import Data.Attoparsec.Frames.Testing
import qualified Data.ByteString as BS
import Data.Word (Word32)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (forAllM, monadicIO, run)


spec :: Spec
spec = describe "ToyFrame" $ do
  context "parser" $
    it "should roundtrip with 'builder'" prop_trip

  mapM_ receivesWithChunksOf [16, 256, 1024, 2048, 4096, 8192]

  context "when input ends, receivesFrames" $ do
    let basic = mkFramer parser (const $ pure ()) $ const $ pure BS.empty
        otherError = setOnClosed (throwIO Underflow) basic
        noError = setOnClosed (pure ()) basic

    it "should use the default close handler" $ do
      receiveFrames basic `shouldThrow` (\NoMoreInput -> True)

    context "when a throwing closed handler is installed" $ do
      it "should throw" $ do
        receiveFrames otherError `shouldThrow` (\x -> x == Underflow)

    context "when a closed handler does not throw an exception" $ do
      it "should finish without throwing" $ do
        receiveFrames noError `shouldReturn` ()


receivesWithChunksOf :: Word32 -> SpecWith ()
receivesWithChunksOf chunkSize' = do
  context ("when chunk size is " ++ show chunkSize') $
    context "receiveFrames" $
      it "should parse into frames" $ prop_receiveFrames chunkSize'


prop_trip :: Property
prop_trip =
  withMaxSuccess 15000 $
    forAll genFullFrame $
      \p -> parse (asBytes p) == Just p


prop_receiveFrames :: Word32 -> Property
prop_receiveFrames chunkSize' = monadicIO $
  forAllM (listOf1 genFullFrame) $
    \ps -> run $ parsesFromFramerOk asBytes parser chunkSize' ps
