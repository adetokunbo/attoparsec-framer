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
import Control.Exception (ArithException (..), catch, throwIO)
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.Frames
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (
  IORef,
  modifyIORef',
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.List (unfoldr)
import Data.Word (Word32, Word8)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (forAllM, monadicIO, run)


spec :: Spec
spec = describe "ToyFrame" $ do
  context "parser" $
    it "should roundtrip with 'builder'" prop_trip

  mapM_ receivesWithChunksOf [16, 256, 1024, 2048, 4096, 8192]

  context "when input ends, receivesFrames" $ do
    let basic = mkFrames parser (const $ pure ()) $ const $ pure BS.empty
        other = setThrowParseFail (\_ -> throwIO Underflow) basic

    it "should use the default errorhandler" $ do
      receiveFrames basic `shouldThrow` (\(BrokenFrame _) -> True)

    context "and when an error handler is installed" $ do
      it "should use the installed errorhandler" $ do
        receiveFrames other `shouldThrow` (\x -> x == Underflow)


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


prop_receiveFrames :: Word32 -> Property
prop_receiveFrames chunkSize' = monadicIO $
  forAllM (listOf1 genFullFrame) $
    \ps -> run $ checkReceiveFrames chunkSize' ps


checkReceiveFrames :: Word32 -> [FullFrame] -> IO Bool
checkReceiveFrames chunkSize' wanted = do
  chunkStore <- newIORef Nothing
  dst <- newIORef []
  let updateDst x = modifyIORef' dst ((:) x)
      mkChunks n = mconcat $ map (chunksOfN n . asBytes) wanted
      src = nextFrom' mkChunks chunkStore
      frames = setChunkSize chunkSize' $ mkFrames parser updateDst src
  receiveFrames frames `catch` (\(_e :: BrokenFrame) -> pure ())
  got <- readIORef dst
  pure $ got == reverse wanted


chunksOfN :: Int -> BS.ByteString -> [BS.ByteString]
chunksOfN x b =
  let go y =
        let taken = BS.take x y
         in if BS.null taken then Nothing else Just (taken, BS.drop x y)
   in unfoldr go b


nextFrom' ::
  (Int -> [BS.ByteString]) -> IORef (Maybe [BS.ByteString]) -> Word32 -> IO BS.ByteString
nextFrom' initChunks chunkStore chunkSize' = do
  readIORef chunkStore >>= \case
    Nothing -> do
      writeIORef chunkStore $ Just $ initChunks $ fromIntegral chunkSize'
      nextFrom' initChunks chunkStore chunkSize'
    Just [] -> pure BS.empty
    Just (x : xs) -> do
      writeIORef chunkStore $ Just xs
      pure x
