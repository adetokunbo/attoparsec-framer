{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Data.Attoparsec.Frames (
  -- * Frames
  mkFrames,
  mkFrames',
  Frames,
  receiveFrame,
  receiveFrames,
  chunkSize,
  setChunkSize,
  setOnBadParse,
  setOnClosed,
  BrokenFrame (..),
  NoMoreInput (..),
  Progression (..),

  -- * Frame size
  FrameSize (..),
  parseSizedFrame,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)


class FrameSize a where
  frameSize :: a -> Word32


-- | Creates an 'A.Parser' that parses a datastructure specifying a frame size, and then a separate framed one with the given size
parseSizedFrame :: FrameSize h => A.Parser h -> A.Parser b -> A.Parser (h, b)
parseSizedFrame parseHead parseBody = do
  h <- parseHead
  let size = frameSize h
  body <- fixed (fromIntegral size) parseBody
  pure (h, body)


fixed :: Word32 -> A.Parser a -> A.Parser a
fixed i p = do
    intermediate <- A.take $ fromIntegral i
    case A.parseOnly (p <* A.endOfInput) intermediate of
        Left x -> fail x
        Right x -> pure x
data Frames m a = Frames
  { framerChunkSize :: !Word32
  , framerOnBadParse :: !(Text -> m ())
  , framerFetchBytes :: !(Word32 -> m ByteString)
  , framerOnFrame :: !(a -> m Progression)
  , framerParser :: !(A.Parser a)
  , framerOnClosed :: !(m ())
  }


mkFrames' ::
  MonadIO m =>
  A.Parser a ->
  (a -> m Progression) ->
  (Word32 -> m ByteString) ->
  Frames m a
mkFrames' parser onFrame fetchBytes =
  Frames
    { framerChunkSize = defaultChunkSize
    , framerOnBadParse = \_err -> pure ()
    , framerFetchBytes = fetchBytes
    , framerOnFrame = onFrame
    , framerParser = parser
    , framerOnClosed = liftIO $ throwIO NoMoreInput
    }


mkFrames ::
  MonadIO m =>
  A.Parser a ->
  (a -> m ()) ->
  (Word32 -> m ByteString) ->
  Frames m a
mkFrames parser onFrame fetchBytes =
  let onFrameContinue x = do
        onFrame x
        pure Continue
   in mkFrames' parser onFrameContinue fetchBytes


receiveFrames ::
  MonadIO m =>
  Frames m a ->
  m ()
receiveFrames f =
  let Frames
        { framerChunkSize = fetchSize
        , framerOnBadParse = onErr
        , framerFetchBytes = fetchBytes
        , framerOnFrame = onFrame
        , framerParser = parser
        , framerOnClosed = onClosed
        } = f
   in receiveFrames' fetchSize parser fetchBytes onFrame onErr onClosed


chunkSize :: Frames m a -> Word32
chunkSize = framerChunkSize


setChunkSize :: Word32 -> Frames m a -> Frames m a
setChunkSize size f = f {framerChunkSize = size}


setOnBadParse :: (Text -> m ()) -> Frames m a -> Frames m a
setOnBadParse onErr f = f {framerOnBadParse = onErr}


setOnClosed :: (m ()) -> Frames m a -> Frames m a
setOnClosed onClose f = f {framerOnClosed = onClose}


receiveFrames' ::
  MonadIO m =>
  Word32 ->
  A.Parser a ->
  (Word32 -> m ByteString) ->
  (a -> m Progression) ->
  (Text -> m ()) ->
  m () ->
  m ()
receiveFrames' fetchSize parser fetchBytes handleFrame onErr onClosed = do
  let loop x = do
        (next, closed) <- receiveFrame' x fetchSize parser fetchBytes handleFrame onErr onClosed
        if not closed then loop next else pure ()
  loop Nothing


receiveFrame ::
  MonadIO m =>
  Maybe ByteString ->
  Frames m a ->
  m ((Maybe ByteString), Bool)
receiveFrame restMb f =
  let Frames
        { framerChunkSize = fetchSize
        , framerOnBadParse = onErr
        , framerFetchBytes = fetchBytes
        , framerOnFrame = onFrame
        , framerParser = parser
        , framerOnClosed = onClose
        } = f
   in receiveFrame' restMb fetchSize parser fetchBytes onFrame onErr onClose


receiveFrame' ::
  MonadIO m =>
  Maybe ByteString ->
  Word32 ->
  A.Parser a ->
  (Word32 -> m ByteString) ->
  (a -> m Progression) ->
  (Text -> m ()) ->
  m () ->
  m ((Maybe ByteString), Bool)
receiveFrame' restMb fetchSize parser fetchBytes handleFrame onErr onClose = do
  let pullChunk = fetchBytes fetchSize
      initial = fromMaybe BS.empty restMb
      onParse (A.Fail _ ctxs reason) = do
        let errMessage = parsingFailed ctxs reason
        if reason == closedReason
          then -- TODO: determine a typed way of detecting this condition, i.e,
          -- it is possible not to rely on a  specific error message ?
          do
            onClose
            pure (Nothing, True)
          else do
            onErr errMessage
            liftIO $ throwIO $ BrokenFrame reason
      onParse (A.Done i r) = do
        let extraMb = if BS.null i then Nothing else Just i
        doMore <- handleFrame r
        case (doMore, extraMb) of
          (Stop, _) -> pure (extraMb, True)
          (StopUnlessExtra, Nothing) -> pure (extraMb, True)
          (_, _) -> pure (extraMb, False)
      onParse (A.Partial continue) = pullChunk >>= onParse . continue
  A.parseWith pullChunk parser initial >>= onParse


parsingFailed :: [String] -> String -> Text
parsingFailed context reason =
  let contexts = Text.intercalate "-" (Text.pack <$> context)
      cause = if null reason then Text.empty else ":" <> Text.pack reason
   in "bad parse:" <> contexts <> cause


newtype BrokenFrame = BrokenFrame String
  deriving (Eq, Show)


instance Exception BrokenFrame


data NoMoreInput = NoMoreInput
  deriving (Eq, Show)


instance Exception NoMoreInput


data Progression
  = Stop
  | StopUnlessExtra
  | Continue
  deriving (Eq, Show)


closedReason :: String
closedReason = "not enough input"


defaultChunkSize :: Word32
defaultChunkSize = 2048
