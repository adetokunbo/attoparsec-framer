{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Data.Attoparsec.Framer.Testing
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

This module provides combinators that simplify unit tests of code that
use @'Framer's@.
-}
module Data.Attoparsec.Framer.Testing (
  -- * testing combinators
  parsesFromFramerOk,
  chunksOfN,
  linkedSrcAndSink,
) where

import Control.Exception (catch)
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.Framer
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (
  IORef,
  modifyIORef',
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.List (unfoldr)
import Data.Word (Word32)


{- | Creates a 'Framer' and uses 'runFramer to confirm that the expect frames
  are received '
-}
parsesFromFramerOk :: Eq a => (a -> ByteString) -> A.Parser a -> Word32 -> [a] -> IO Bool
parsesFromFramerOk asBytes parser chunkSize' wanted = do
  chunkStore <- newIORef Nothing
  dst <- newIORef []
  let updateDst x = modifyIORef' dst ((:) x)
      mkChunks n = mconcat $ map (chunksOfN n . asBytes) wanted
      src = nextFrom' mkChunks chunkStore
      frames = setChunkSize chunkSize' $ mkFramer parser updateDst src
  runFramer frames `catch` (\(_e :: NoMoreInput) -> pure ())

  got <- readIORef dst
  pure $ got == reverse wanted


-- | Split a 'ByteString' into chunks of given size
chunksOfN :: Int -> ByteString -> [ByteString]
chunksOfN x b =
  let go y =
        let taken = BS.take x y
         in if BS.null taken then Nothing else Just (taken, BS.drop x y)
   in unfoldr go b


nextFrom' ::
  (Int -> [ByteString]) -> IORef (Maybe [ByteString]) -> Word32 -> IO ByteString
nextFrom' initChunks chunkStore chunkSize' = do
  readIORef chunkStore >>= \case
    Nothing -> do
      writeIORef chunkStore $ Just $ initChunks $ fromIntegral chunkSize'
      nextFrom' initChunks chunkStore chunkSize'
    Just [] -> pure BS.empty
    Just (x : xs) -> do
      writeIORef chunkStore $ Just xs
      pure x


{- | A @'ByteSource'@ linked to a byte sink.

Provides a @ByteSource@ and @byte sink@ that emulate a responding endpoint.

The @responses@ are consumed each time the byte sink is invoked.

Whenever the sink is invoked, the head of the provided responses is removed
and starts to be returned in chunks by the @ByteSource@,
-}
linkedSrcAndSink :: [ByteString] -> IO (ByteSource IO, (ByteString -> IO ()))
linkedSrcAndSink responses = do
  refSrc <- newIORef Nothing
  refSink <- newIORef responses
  pure (ioRefByteSource refSrc, ioRefByteSink refSink refSrc)


ioRefByteSource :: IORef (Maybe ByteString) -> ByteSource IO
ioRefByteSource refSrc size = do
  readIORef refSrc >>= \case
    Nothing -> pure BS.empty
    Just src -> do
      let taken = BS.take (fromIntegral size) src
          rest = BS.drop (fromIntegral size) src
          stored = if BS.null rest then Nothing else Just rest
      writeIORef refSrc stored
      pure taken


ioRefByteSink :: IORef [ByteString] -> IORef (Maybe ByteString) -> ByteString -> IO ()
ioRefByteSink refResponses refSrc _ignored = do
  readIORef refResponses >>= \case
    [] -> writeIORef refSrc Nothing
    (x : xs) -> do
      writeIORef refSrc $ Just x
      writeIORef refResponses $ xs
