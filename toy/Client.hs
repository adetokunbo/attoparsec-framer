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
  Framer,
  mkFramer,
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
  trackFrames trackingRef (socketSink s) Nothing
  receiveFrames $ fromSocket trackingRef s
  tracking <- readIORef trackingRef
  putStrLn $ "Received " ++ (show $ trackingFrames tracking) ++ " of total size " ++ (show $ trackingBytes tracking)


fromSocket :: IORef Tracking -> Socket -> Framer IO FullFrame
fromSocket ref s =
  let sink = socketSink s
      onFullFrame' f = trackFrames ref sink $ Just f
   in setOnClosed onClosed $
        setOnBadParse (onFailedParse' sink) $
          mkFramer parser onFullFrame' (recv s . fromIntegral)


data Tracking = Tracking
  { trackingLeft :: ![Header]
  , trackingBytes :: !Int
  , trackingFrames :: !Int
  , trackingCountdown :: !(Int, Int)
  }


newTrackingRef :: [Header] -> IO (IORef Tracking)
newTrackingRef xs = newIORef $ Tracking xs 0 0 (0, 0)


trackFrames :: IORef Tracking -> ByteSink -> Maybe FullFrame -> IO ()
trackFrames trackingRef sink frameMb = do
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
      sink bye
    (Just (_, Payload p'), x : xs) -> incrOr p' $ do
      let updatedTracking =
            (incrWithPayload p')
              { trackingCountdown = (fromIntegral $ hIndex x, 0)
              , trackingLeft = xs
              }
      writeIORef trackingRef updatedTracking
      sink $ asBytes x
    (Nothing, x : xs) -> do
      writeIORef
        trackingRef
        t
          { trackingCountdown = (fromIntegral $ hIndex x, 0)
          , trackingLeft = xs
          }
      sink $ asBytes x
    (Nothing, []) -> sink bye


type ByteSink = BS.ByteString -> IO ()


socketSink :: Socket -> ByteSink
socketSink = sendAll


bye :: BS.ByteString
bye = asBytes $ Header 0 0


onFailedParse' :: ByteSink -> Text -> IO ()
onFailedParse' sink cause = do
  -- if does not parse as a full frame immediately terminate the connection
  Text.putStrLn $ "parse error ended a connection to a toy server: " <> cause
  sink bye


onClosed :: IO ()
onClosed = Text.putStrLn "finished at the server too!"


asBytes :: Header -> BS.ByteString
asBytes = LBS.toStrict . toLazyByteString . buildFrameHeader
