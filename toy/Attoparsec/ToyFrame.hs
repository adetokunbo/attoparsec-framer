{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Attoparsec.ToyFrame (
  -- * data types
  Header (..),
  Payload (Payload),
  FullFrame,

  -- * functions
  asBytes,
  builder,
  buildFrameHeader,
  parse,
  parser,
  parseHeader,

  -- * sample data
  genPayload,
  genAscFullFrames,
  genHeader,
  genFullFrame,
  someTriggers,
) where

import qualified Data.Attoparsec.Binary as A
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word32, Word8)
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  chooseEnum,
  generate,
  vectorOf,
 )


-- | Class for datastructures that specify the frame size of a payload; used by 'parseSizedFrame'
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


type FullFrame = (Header, Payload)


data Header = Header
  { hIndex :: !Word32
  , hSize :: !Word32
  }
  deriving (Eq, Show)


instance FrameSize Header where
  frameSize = hSize


newtype Payload = Payload ByteString
  deriving (Eq, Show)


parseHeader :: A.Parser Header
parseHeader = Header <$> A.anyWord32be <*> A.anyWord32be


buildFrameHeader :: Header -> Builder
buildFrameHeader fh = word32BE (hIndex fh) <> word32BE (hSize fh)


parseFrame :: A.Parser Payload
parseFrame = fmap Payload $ A.takeByteString


parser :: A.Parser FullFrame
parser = parseSizedFrame parseHeader parseFrame


builder' :: Word32 -> Payload -> Builder
builder' hIndex (Payload b) =
  let hSize = fromIntegral $ BS.length b
      header = Header {hIndex, hSize}
   in buildFrameHeader header <> byteString b


builder :: FullFrame -> Builder
builder (header, body) = builder' (hIndex header) body


asBytes :: FullFrame -> BS.ByteString
asBytes = LBS.toStrict . toLazyByteString . builder


parse :: BS.ByteString -> Maybe FullFrame
parse = A.maybeResult . A.parse parser


genPrintable :: Gen Word8
genPrintable = chooseEnum (32, 127)


genPayload :: Word32 -> Gen Payload
genPayload size = fmap (Payload . BS.pack) $ vectorOf (fromIntegral size) genPrintable


genHeader :: Gen Header
genHeader = Header <$> arbitrary <*> chooseEnum (2, 32)


genClientTrigger :: Gen Header
genClientTrigger = Header <$> (chooseEnum (32, 4096)) <*> (chooseEnum (32, 1024))


someTriggers :: Int -> IO [Header]
someTriggers count = generate $ vectorOf count $ genClientTrigger


genFullFrame :: Gen FullFrame
genFullFrame = do
  header <- genHeader
  body <- genPayload $ hSize header
  pure (header, body)


genEnumPayload' :: Word32 -> Word32 -> Gen [(Word32, Payload)]
genEnumPayload' count maxSize = vectorOf (fromIntegral count) $ do
  aSize <- chooseEnum (1, maxSize)
  payload <- genPayload aSize
  pure (aSize, payload)


genAscFullFrames :: Word32 -> Word32 -> IO [FullFrame]
genAscFullFrames count maxSize = generate $ do
  xs <- genEnumPayload' count maxSize
  let toFullFrame (hIndex, (hSize, p)) = (Header {hSize, hIndex}, p)
  pure $ map toFullFrame $ zip [1 ..] xs
