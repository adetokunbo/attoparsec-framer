{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Data.Attoparsec.Frames.Testing (
  -- * testing combinators
  parsesFromFramesOk,
  chunksOfN,
) where

import Control.Exception (catch)
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.Frames
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


parsesFromFramesOk :: Eq a => (a -> ByteString) -> A.Parser a -> Word32 -> [a] -> IO Bool
parsesFromFramesOk asBytes parser chunkSize' wanted = do
  chunkStore <- newIORef Nothing
  dst <- newIORef []
  let updateDst x = modifyIORef' dst ((:) x)
      mkChunks n = mconcat $ map (chunksOfN n . asBytes) wanted
      src = nextFrom' mkChunks chunkStore
      frames = setChunkSize chunkSize' $ mkFrames parser updateDst src
  receiveFrames frames `catch` (\(_e :: BrokenFrame) -> pure ())
  got <- readIORef dst
  pure $ got == reverse wanted


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
