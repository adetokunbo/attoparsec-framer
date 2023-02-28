{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Main (main) where

import Attoparsec.ToyFrame (Header (..), asBytes, genAscFullFrames, parseHeader)
import Data.Attoparsec.Framer (
  Framer,
  Progression (..),
  mkFramer',
  receiveFrames,
  setOnBadParse,
  setOnClosed,
 )
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Network.Run.TCP (runTCPServer)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)


main :: IO ()
main = runTCPServer Nothing "3927" $ \s -> do
  Text.putStrLn "a toy client connected"
  receiveFrames $ fromSocket s


type ByteSink = BS.ByteString -> IO ()


fromSocket :: Socket -> Framer IO Header
fromSocket s =
  let onHeader' = onHeader $ sendAll s
   in setOnClosed onClosed $
        setOnBadParse onFailedParse $
          mkFramer' parseHeader onHeader' (recv s . fromIntegral)


onHeader :: ByteSink -> Header -> IO Progression
onHeader sink Header {hIndex, hSize} = do
  if (hIndex == 0)
    then -- hIndex is 0; the client means 'bye', stop waiting for input
    do
      Text.putStrLn "a toy client sent bye"
      pure Stop
    else do
      -- hIndex > 0; starting from 1, send a frame with a body whose max size is hSize
      -- generate a list of frames counting up to the index provided in the header
      toSend <- genAscFullFrames hIndex hSize
      mapM_ sink $ map asBytes toSend
      pure Continue


onFailedParse :: Text -> IO ()
onFailedParse cause = do
  -- if does not parse as a frame header immediately terminate the connection
  Text.putStrLn $ "parse error ended a connection from a toy client: " <> cause


onClosed :: IO ()
onClosed = Text.putStrLn "a toy client closed a connection"
