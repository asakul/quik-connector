
module QuoteSource.Server (
  startQuoteSourceServer,
  stopQuoteSourceServer
) where

import System.ZMQ4
import Control.Concurrent.BoundedChan
import Data.ATrade
import Control.Concurrent hiding (readChan)
import Control.Monad
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty hiding (map)
import System.Log.Logger

data QuoteSourceServer = QuoteSourceServerState {
  ctx :: Context,
  outSocket :: Socket Pub,
  tickChannel :: BoundedChan Tick,
  serverThreadId :: ThreadId
}

serverThread :: QuoteSourceServer -> IO ()
serverThread state = do
  finally serverThread' cleanup
  debugM "QuoteSource" "server thread done"
  where
    cleanup = close $ outSocket state

    serverThread' = forever $ do
      tick <- readChan $ tickChannel state
      sendMulti (outSocket state) $ fromList . map BL.toStrict $ serializeTick tick

startQuoteSourceServer :: BoundedChan Tick -> Context -> String -> IO QuoteSourceServer
startQuoteSourceServer chan c ep = do
  sock <- socket c Pub
  bind sock ep
  tid <- myThreadId
  let state = QuoteSourceServerState {
    ctx = c,
    outSocket = sock,
    tickChannel = chan,
    serverThreadId = tid
  }
  stid <- forkIO $ serverThread state
  return $ state { serverThreadId = stid }

stopQuoteSourceServer :: QuoteSourceServer -> IO ()
stopQuoteSourceServer server = killThread $ serverThreadId server

