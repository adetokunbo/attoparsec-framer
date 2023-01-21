{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Main (main) where

import Attoparsec.ToyFrame (Header (..), asBytes, genAscFullFrames, parseHeader)
import Data.Attoparsec.Frames (Frames, mkFrames, receiveFrames, setOnBadParse)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Network.Run.TCP (runTCPServer)
import Network.Socket (Socket, gracefulClose)
import Network.Socket.ByteString (recv, sendAll)


main :: IO ()
main = runTCPServer Nothing "3000" $ receiveFrames . fromSocket


fromSocket :: Socket -> Frames IO Header
fromSocket s =
  setOnBadParse (onFailedParse s) $
    mkFrames parseHeader (onHeader s) (recv s . fromIntegral)


onHeader :: Socket -> Header -> IO ()
onHeader s Header {hIndex, hSize} = do
  if (hIndex == 0)
    then -- if hIndex is 0 close the socket
      gracefulClose s 5000
    else do
      -- when hIndex is 0 > start from 1, send a frame with a body whose max size is hSize
      -- generate a list of frames counting up to the index provided in the header
      toSend <- genAscFullFrames hIndex hSize
      mapM_ (sendAll s) $ map asBytes toSend


onFailedParse :: Socket -> Text -> IO ()
onFailedParse s cause = do
  -- if does not parse as a frame header immediately terminate the connection
  Text.putStrLn $ "error ended a connection to a toy client: " <> cause
  gracefulClose s 5000
