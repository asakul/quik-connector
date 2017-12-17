{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

module ReplayServer (
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import Safe

import System.ZMQ4
import System.Log.Logger

type ShutdownMVar = MVar ()

data Request = StartRecording FilePath | StopRecording | StartReplay FilePath | StopReplay
data Response = ResponseOk | ResponseError T.Text

instance FromJSON Request where
  parseJSON (Object v) = do
    rq <- v .: "request"
    if
      | rq == ("start-recording" :: T.Text) -> StartRecording <$> v.: "filename"
      | otherwise -> fail "Unknown request"
  parseJSON invalid = typeMismatch "Request" invalid

instance ToJSON Response where
  toJSON ResponseOk = object ["response" .= ("ok" :: T.Text)]
  toJSON (ResponseError errmsg) = object ["response" .= ("error" :: T.Text), "message" .= errmsg]

startReplayServer :: Context -> T.Text -> IO ShutdownMVar
startReplayServer ctx ep = do
  shutdownMVar <- newEmptyMVar
  _ <- forkIO $ replayServerEventLoop shutdownMVar ctx ep
  return shutdownMVar

stopReplayServer :: ShutdownMVar -> IO ()
stopReplayServer mv = void $ tryPutMVar mv ()

replayServerEventLoop shutdownMVar ctx ep = withSocket ctx Rep (\sock -> do
  events <- poll 1000 [Sock sock [In] Nothing]
  when (isJust $ headMay events >>= headMay) $ do
    rawMsg <- receive sock
    case eitherDecode (BL.fromStrict rawMsg) of
      Right msg -> handle msg >>= send sock [] . BL.toStrict . encode
      Left errmsg -> debugM "ReplayServer" $ "Got invalid command: " ++ errmsg)
  where
    handle :: Request -> IO Response
    handle = undefined
