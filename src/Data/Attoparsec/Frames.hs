{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  chunkSize,
  setChunkSize,
  setOnBadParse,
  setOnClosed,
  setOnFrame,
  BrokenFrame (..),
  NoMoreInput (..),
  Progression (..),
  receiveFrame,
  receiveFrames,
) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)


-- | Handles a parsed @frame@, returning a @Progression@ to indicate if further @frames@ should be parsed.
type FrameHandler m frame = frame -> m Progression


-- | Used by 'FrameHandler' to indicate if additional frames should be parsed.
data Progression
  = Stop
  | StopUnlessExtra
  | Continue
  deriving (Eq, Show)


-- | Use an 'A.Parser' to parse a stream of datastructures from a series of byte chunks
data Frames m a = Frames
  { framerChunkSize :: !Word32
  , framerOnBadParse :: !(Text -> m ())
  , framerNextChunk :: !(Word32 -> m ByteString)
  , framerOnFrame :: !(FrameHandler m a)
  , framerParser :: !(A.Parser a)
  , framerOnClosed :: !(m ())
  }


mkFrames' ::
  MonadThrow m =>
  A.Parser a ->
  FrameHandler m a ->
  (Word32 -> m ByteString) ->
  Frames m a
mkFrames' parser onFrame fetchBytes =
  Frames
    { framerChunkSize = defaultChunkSize
    , framerOnBadParse = \_err -> pure ()
    , framerNextChunk = fetchBytes
    , framerOnFrame = onFrame
    , framerParser = parser
    , framerOnClosed = throwM NoMoreInput
    }


mkFrames ::
  MonadThrow m =>
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
  MonadThrow m =>
  Frames m a ->
  m ()
receiveFrames f =
  let Frames
        { framerChunkSize = fetchSize
        , framerOnBadParse = onErr
        , framerNextChunk = fetchBytes
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


-- | Update the @FrameHandler@ of a @Frames@.
setOnFrame :: FrameHandler m frame -> Frames m frame -> Frames m frame
setOnFrame onFrame f = f {framerOnFrame = onFrame}


setOnClosed :: (m ()) -> Frames m a -> Frames m a
setOnClosed onClose f = f {framerOnClosed = onClose}


receiveFrames' ::
  MonadThrow m =>
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
  MonadThrow m =>
  Maybe ByteString ->
  Frames m a ->
  m ((Maybe ByteString), Bool)
receiveFrame restMb f =
  let Frames
        { framerChunkSize = fetchSize
        , framerOnBadParse = onErr
        , framerNextChunk = fetchBytes
        , framerOnFrame = onFrame
        , framerParser = parser
        , framerOnClosed = onClose
        } = f
   in receiveFrame' restMb fetchSize parser fetchBytes onFrame onErr onClose


receiveFrame' ::
  MonadThrow m =>
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
            throwM $ BrokenFrame reason
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


closedReason :: String
closedReason = "not enough input"


defaultChunkSize :: Word32
defaultChunkSize = 2048
