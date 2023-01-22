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
  someTriggers,
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
  trackingRef <- someTriggers 1024 >>= newTrackingRef
  trackFrames trackingRef s Nothing
  receiveFrames $ fromSocket trackingRef s
  tracking <- readIORef trackingRef
  putStrLn $ "Received " ++ (show $ trackingFrames tracking) ++ " of total size " ++ (show $ trackingBytes tracking)


fromSocket :: IORef Tracking -> Socket -> Frames IO FullFrame
fromSocket ref s =
  setOnClosed onClosed $
    setOnBadParse (onFailedParse s) $
      mkFrames parser (onFullFrame ref s) (recv s . fromIntegral)


onFullFrame :: IORef Tracking -> Socket -> FullFrame -> IO ()
onFullFrame ref socket frame = trackFrames ref socket $ Just frame


data Tracking = Tracking
  { trackingLeft :: ![Header]
  , trackingBytes :: !Int
  , trackingFrames :: !Int
  , trackingCountdown :: !(Int, Int)
  }


newTrackingRef :: [Header] -> IO (IORef Tracking)
newTrackingRef xs = newIORef $ Tracking xs 0 0 (0, 0)


trackFrames :: IORef Tracking -> Socket -> Maybe FullFrame -> IO ()
trackFrames trackingRef socket frameMb = do
  t <- readIORef trackingRef
  let (target, lastCount) = trackingCountdown t
      nextCount = lastCount + 1
      nextFrames = trackingFrames t + 1
      incrWithPayload p =
        t
          { trackingCountdown = (target, nextCount)
          , trackingFrames = nextFrames
          , trackingBytes = trackingBytes t + BS.length p
          }
      countedUp = nextCount == target
      incrOr p' action =
        if not countedUp
          then writeIORef trackingRef $ incrWithPayload p'
          else action

  case (frameMb, trackingLeft t) of
    (Just (_, Payload p'), []) -> incrOr p' $ do
      writeIORef trackingRef $ incrWithPayload p'
      sendBye socket
    (Just (_, Payload p'), x : xs) -> incrOr p' $ do
      let updatedTracking =
            (incrWithPayload p')
              { trackingCountdown = (fromIntegral $ hIndex x, 0)
              , trackingLeft = xs
              }
      writeIORef trackingRef updatedTracking
      sendAll socket $ asBytes x
    (Nothing, x : xs) -> do
      writeIORef
        trackingRef
        t
          { trackingCountdown = (fromIntegral $ hIndex x, 0)
          , trackingLeft = xs
          }
      sendAll socket $ asBytes x
    (Nothing, []) -> sendBye socket


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
