{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Attoparsec.ToyFrameSpec
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Attoparsec.ToyFrameSpec (spec) where

import Attoparsec.ToyFrame
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word32, Word8)
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = describe "ToyFrame" $ do
  context "parser" $
    it "should roundtrip with 'builder'" prop_trip


prop_trip :: Property
prop_trip =
  withMaxSuccess 15000 $
    forAll genFullFrame $
      \p -> parse (asBytes p) == Just p


genPrintable :: Gen Word8
genPrintable = chooseEnum (32, 127)


genFrameBody :: Word32 -> Gen FrameBody
genFrameBody size = fmap (FrameBody . BS.pack) $ vectorOf (fromIntegral size) genPrintable


genFrameHeader :: Gen FrameHeader
genFrameHeader = FrameHeader <$> arbitrary <*> chooseEnum (2, 32)


genFullFrame :: Gen FullFrame
genFullFrame = do
  header <- genFrameHeader
  body <- genFrameBody $ fhSize header
  pure (header, body)


asBytes :: FullFrame -> BS.ByteString
asBytes = LBS.toStrict . toLazyByteString . builder


parse :: BS.ByteString -> Maybe FullFrame
parse = A.maybeResult . A.parse parser
