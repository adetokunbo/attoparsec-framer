{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Attoparsec.ToyFrame (
  -- * data types
  FrameHeader (..),
  FrameBody (FrameBody),
  FullFrame,

  -- * functions
  builder,
  parser,
) where

import qualified Data.Attoparsec.Binary as A
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.Frames (FrameSize (..), parseSizedFrame)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.Word (Word32)


type FullFrame = (FrameHeader, FrameBody)


data FrameHeader = FrameHeader
  { fhIndex :: !Word32
  , fhSize :: !Word32
  }
  deriving (Eq, Show)


instance FrameSize FrameHeader where
  frameSize = fhSize


newtype FrameBody = FrameBody ByteString
  deriving (Eq, Show)


parseFrameHeader :: A.Parser FrameHeader
parseFrameHeader = FrameHeader <$> A.anyWord32be <*> A.anyWord32be


buildFrameHeader :: FrameHeader -> Builder
buildFrameHeader fh = word32BE (fhIndex fh) <> word32BE (fhSize fh)


parseFrame :: Word32 -> A.Parser FrameBody
parseFrame pageSize = fmap FrameBody $ A.take $ fromIntegral pageSize


parser :: A.Parser FullFrame
parser = parseSizedFrame parseFrameHeader parseFrame


builder' :: Word32 -> FrameBody -> Builder
builder' fhIndex (FrameBody b) =
  let fhSize = fromIntegral $ BS.length b
      header = FrameHeader {fhIndex, fhSize}
   in buildFrameHeader header <> byteString b


builder :: FullFrame -> Builder
builder (header, body) = builder' (fhIndex header) body
