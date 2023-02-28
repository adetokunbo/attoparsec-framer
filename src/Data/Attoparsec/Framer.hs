{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module : Data.Attoparsec.Framer
Copyright : (c) 2022 Tim Emiola
Maintainer : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides the 'Framer' data type that combines an @Attoparsec 'A.Parser'@ with a
a few additional combinators that allow the parser to be used to process frames
from the kind of framed byte streams commonly used to implement networking
protocols.

A @'Framer'@ specifies how the framing function @'receiveFrames'@ should parse a
byte stream.

Minimally, a @Framer@ specifies

* An @'A.Parser'@, used to extract frames from the byte stream
* a @'FrameHandler'@ responsible using the parsed frames
* the bytestream source, represented by combinator that obtains the next chunk from the source


Within @'receiveFrames'@ the 'FrameHandler' is invoked repeatedly; on each
invocation it returns a 'Progression', which indicates if processing should
continue. This makes it possible to terminate for the 'FrameHandler' to signal
that frame processing should terminate.


-}
module Data.Attoparsec.Framer (
  -- * Framer
  Framer,
  FrameHandler,
  Progression (..),
  mkFramer,
  mkFramer',

  -- * query/update a  @'Framer'@
  setChunkSize,
  setOnBadParse,
  setOnClosed,
  setOnFrame,
  chunkSize,

  -- * run the @FrameHandler@
  receiveFrame,
  receiveFrames,

  -- * potential handling exceptions
  BrokenFrame (..),
  NoMoreInput (..),
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


-- | Handles a parsed @frame@, returning a @Progression@ that indicates if further @frames@ should be parsed.
type FrameHandler m frame = frame -> m Progression


-- | Used by 'FrameHandler' to indicate if additional frames should be parsed.
data Progression
   = Stop
  | StopUnlessExtra
  | Continue
  deriving (Eq, Show)


-- | Use 'A.Parser' to parse a stream of @frames@ from a bytestream
data Framer m frame = Framer
  { framerChunkSize :: !Word32
  , framerOnBadParse :: !(Text -> m ())
  , framerNextChunk :: !(Word32 -> m ByteString)
  , framerOnFrame :: !(FrameHandler m frame)
  , framerParser :: !(A.Parser frame)
  , framerOnClosed :: !(m ())
  }


{- | Construct @'Framer'@ that will handle @frames@ repeatedly until a returned
 @'Progression'@ stops it.
-}
mkFramer' ::
  MonadThrow m =>
  A.Parser frame ->
  FrameHandler m frame ->
  (Word32 -> m ByteString) ->
  Framer m frame
mkFramer' framerParser framerOnFrame framerNextChunk =
  Framer
    { framerParser
    , framerOnFrame
    , framerNextChunk
    , framerOnBadParse = \_err -> pure ()
    , framerOnClosed = throwM NoMoreInput
    , framerChunkSize = defaultChunkSize
    }


-- | Construct @'Framer'@ that loops continuously.
mkFramer ::
  MonadThrow m =>
  A.Parser a ->
  (a -> m ()) ->
  (Word32 -> m ByteString) ->
  Framer m a
mkFramer parser onFrame fetchBytes =
  let onFrameContinue x = do
        onFrame x
        pure Continue
   in mkFramer' parser onFrameContinue fetchBytes


-- | Repeatedly parse and handle frames until the configured @FrameHandler@ ends handling.
receiveFrames ::
  MonadThrow m =>
  Framer m a ->
  m ()
receiveFrames f =
  let Framer
        { framerChunkSize = fetchSize
        , framerOnBadParse = onErr
        , framerNextChunk = fetchBytes
        , framerOnFrame = onFrame
        , framerParser = parser
        , framerOnClosed = onClosed
        } = f
   in receiveFrames' fetchSize parser fetchBytes onFrame onErr onClosed


{- | Parse and handle a single frame.

The result is tuple of the outstanding unparsed bytes from the bytestream if
any, and a value indicating if the bytestream has terminated.
-}
receiveFrame ::
  MonadThrow m =>
  Maybe ByteString ->
  Framer m a ->
  m ((Maybe ByteString), Bool)
receiveFrame restMb f =
  let Framer
        { framerChunkSize = fetchSize
        , framerOnBadParse = onErr
        , framerNextChunk = fetchBytes
        , framerOnFrame = onFrame
        , framerParser = parser
        , framerOnClosed = onClose
        } = f
   in receiveFrame' restMb fetchSize parser fetchBytes onFrame onErr onClose


chunkSize :: Framer m a -> Word32
chunkSize = framerChunkSize


setChunkSize :: Word32 -> Framer m a -> Framer m a
setChunkSize size f = f {framerChunkSize = size}


setOnBadParse :: (Text -> m ()) -> Framer m a -> Framer m a
setOnBadParse onErr f = f {framerOnBadParse = onErr}


-- | Update the @FrameHandler@ of a @Framer@.
setOnFrame :: FrameHandler m frame -> Framer m frame -> Framer m frame
setOnFrame onFrame f = f {framerOnFrame = onFrame}


setOnClosed :: (m ()) -> Framer m a -> Framer m a
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


{- | Why MonadThrow instead of MonadError ?

receiveFrames is parsing framed protocol streams this will usually be done in a
client or server library

MonadThrow is used so that the onFrame, onBadParse and onClose can raise library
exception types from the client/server library classes directly whenever
necessary.

I.e, no specially handling of exceptions that occur during onFrame, and if onErr
does not itself throw an exception, 'BrokenFrame' is thrown.

The caller has created the Framer and provided it with onFrame, onErr and
onClose, and should ensure that whatever is necessary on exceptions during
onFrame are handle appropriately.
-}
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
          then -- WANTED: a typed way of detecting this condition, i.e,
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


-- | Thrown by 'receiveFrames' or 'receiveFrame' if the parsing fails
newtype BrokenFrame = BrokenFrame String
  deriving (Eq, Show)


instance Exception BrokenFrame


{- | Thrown by 'receiveFrames' or 'receiveFrame' when no further input is available
 and @setOnClosed@ was not used.
-}
data NoMoreInput = NoMoreInput
  deriving (Eq, Show)


instance Exception NoMoreInput


closedReason :: String
closedReason = "not enough input"


defaultChunkSize :: Word32
defaultChunkSize = 2048
