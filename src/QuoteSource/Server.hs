
module QuoteSource.Server (
) where

import System.ZMQ4
import Control.Concurrent.BoundedChan
import Data.ATrade
import Control.Concurrent
import Control.Monad

data QuoteSourceServer = QuoteSourceServerState {
  ctx :: Context,
  outSocket :: Socket Pub,
  tickChannel :: BoundedChan Tick,
  serverThread :: ThreadId
}

serverThread :: QuoteSourceServer -> IO ()
serverThread state = do
  finally serverThread' cleanup
  where
    cleanup = close $ outSocket state

    serverThread = forever $ do
      tick <- readChan $ tickChannel state
      sendMulti (outSocket state) serializeTick tick

startQuoteSourceServer :: BoundedChan Tick -> Context -> String -> IO QuoteSourceServer
startQuoteSourceServer chan c ep = do
  sock <- socket c Pub
  bind sock ep
  tid <- myThreadId
  let state = QuoteSourceServerState {
    ctx = c,
    outSocket = sock,
    tickChannel = chan,
    serverThread = tid
  }
  stid <- forkIO $ serverThread state
  return $ state { serverThread = stid }

