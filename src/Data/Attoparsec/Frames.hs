{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Data.Attoparsec.Frames (
  -- * Frames
  mkFrames,
  Frames,
  receiveFrame,
  receiveFrames,
  chunkSize,
  setChunkSize,
  setThrowParseFail,

  -- * Frame size
  FrameSize (..),
  parseSizedFrame,
) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word32, Word64, Word8)


class FrameSize a where
  frameSize :: a -> Word32


parseSizedFrame :: FrameSize h => A.Parser h -> (Word32 -> A.Parser b) -> A.Parser (h, b)
parseSizedFrame parseHead mkParseBody = do
  h <- parseHead
  body <- mkParseBody $ frameSize h
  pure (h, body)


data Frames m a = Frames
  { framerChunkSize :: !Word32
  , framerThrowParseFail :: !(Text -> m ())
  , framerFetchBytes :: !(Word32 -> m ByteString)
  , framerOnFrame :: !(a -> m ())
  , framerParser :: !(A.Parser a)
  }


mkFrames ::
  Applicative m =>
  A.Parser a ->
  (a -> m ()) ->
  (Word32 -> m ByteString) ->
  Frames m a
mkFrames parser onFrame fetchBytes =
  Frames
    { framerChunkSize = 2048
    , framerThrowParseFail = \_err -> pure ()
    , framerFetchBytes = fetchBytes
    , framerOnFrame = onFrame
    , framerParser = parser
    }


receiveFrames ::
  MonadIO m =>
  Frames m a ->
  m ()
receiveFrames f =
  let Frames
        { framerChunkSize = fetchSize
        , framerThrowParseFail = onErr
        , framerFetchBytes = fetchBytes
        , framerOnFrame = onFrame
        , framerParser = parser
        } = f
   in receiveFrames' fetchSize parser fetchBytes onFrame onErr


chunkSize :: Frames m a -> Word32
chunkSize = framerChunkSize


setChunkSize :: Word32 -> Frames m a -> Frames m a
setChunkSize size f = f {framerChunkSize = size}


setThrowParseFail :: (Text -> m ()) -> Frames m a -> Frames m a
setThrowParseFail onErr f = f {framerThrowParseFail = onErr}


receiveFrames' ::
  MonadIO m =>
  Word32 ->
  A.Parser a ->
  (Word32 -> m ByteString) ->
  (a -> m ()) ->
  (Text -> m ()) ->
  m ()
receiveFrames' fetchSize parser fetchBytes handleFrame onErr = do
  let loop x = do
        next <- receiveFrame' x fetchSize parser fetchBytes handleFrame onErr
        loop next
  loop Nothing


receiveFrame ::
  MonadIO m =>
  Maybe ByteString ->
  Frames m a ->
  m (Maybe ByteString)
receiveFrame restMb f =
  let Frames
        { framerChunkSize = fetchSize
        , framerThrowParseFail = onErr
        , framerFetchBytes = fetchBytes
        , framerOnFrame = onFrame
        , framerParser = parser
        } = f
   in receiveFrame' restMb fetchSize parser fetchBytes onFrame onErr


receiveFrame' ::
  MonadIO m =>
  Maybe ByteString ->
  Word32 ->
  A.Parser a ->
  (Word32 -> m ByteString) ->
  (a -> m ()) ->
  (Text -> m ()) ->
  m (Maybe ByteString)
receiveFrame' restMb fetchSize parser fetchBytes handleFrame onErr = do
  let pullChunk = fetchBytes fetchSize
      initial = fromMaybe BS.empty restMb
      onParse (A.Fail _ ctxs reason) = do
        let errMessage = parsingFailed ctxs reason
        onErr errMessage
        liftIO $ throwIO $ userError reason
      onParse (A.Done i r) = do
        handleFrame r
        pure (if BS.null i then Nothing else Just i)
      onParse (A.Partial continue) = pullChunk >>= onParse . continue
  A.parseWith pullChunk parser initial >>= onParse


parsingFailed :: [String] -> String -> Text
parsingFailed context reason =
  let contexts = Text.intercalate "-" (Text.pack <$> context)
      cause = if null reason then Text.empty else ":" <> Text.pack reason
   in "bad parse:" <> contexts <> cause
