{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Main (main) where

import Attoparsec.ToyFrame (
  FullFrame,
  Header (..),
  Payload (..),
  buildFrameHeader,
  parser,
 )
import Data.Attoparsec.Frames (
  Frames,
  mkFrames,
  receiveFrames,
  setOnBadParse,
  setOnClosed,
 )
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (
  IORef,
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Network.Run.TCP (runTCPClient)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)


main :: IO ()
main = runTCPClient "127.0.0.1" "3927" $ \s -> do
  counter <- newIORef (0, 0)
  let trigger = Header 20 1024
  sendAll s $ asBytes trigger
  receiveFrames $ fromSocket 20 counter s
  (count, size) <- readIORef counter
  putStrLn $ "Received " ++ (show count) ++ " of total size " ++ show size


fromSocket :: Int -> IORef (Int, Int) -> Socket -> Frames IO FullFrame
fromSocket maxCount counter s =
  setOnClosed onClosed $
    setOnBadParse (onFailedParse s) $
      mkFrames parser (onFullFrame maxCount counter s) (recv s . fromIntegral)


onFullFrame :: Int -> IORef (Int, Int) -> Socket -> FullFrame -> IO ()
onFullFrame maxCount counter s (_, Payload b) = do
  (count, size) <- readIORef counter
  let updated = (count + 1, size + BS.length b)
  writeIORef counter updated
  putStrLn $ "Count is now " ++ show updated
  if count + 1 >= maxCount then sendBye s else pure ()


onFailedParse :: Socket -> Text -> IO ()
onFailedParse s cause = do
  -- if does not parse as a full frame immediately terminate the connection
  Text.putStrLn $ "parse error ended a connection to a toy server: " <> cause
  sendBye s


sendBye :: Socket -> IO ()
sendBye s = sendAll s $ asBytes $ Header 0 0


onClosed :: IO ()
onClosed = Text.putStrLn "finished at the server too!"


asBytes :: Header -> BS.ByteString
asBytes = LBS.toStrict . toLazyByteString . buildFrameHeader
