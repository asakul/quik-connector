{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module QuoteSource.PipeReader (
  startPipeReader,
  stopPipeReader
  ) where

import           ATrade.Logging                 (Message, Severity (..),
                                                 logWith)
import           ATrade.QuoteSource.Server
import           ATrade.Types
import           Colog                          (LogAction)
import           Control.Applicative
import           Control.Concurrent             hiding (readChan, writeChan,
                                                 writeList2Chan, yield)
import           Control.Concurrent.BoundedChan
import           Control.Error.Util
import           Control.Exception
import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Loops            (whileM_)
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import           Data.Conduit                   hiding (connect)
import qualified Data.Conduit.List              as CL
import qualified Data.HashSet                   as HS
import           Data.IORef
import qualified Data.Map.Strict                as M
import           Data.Maybe
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Data.Time.Calendar
import           Data.Time.Clock
import           Foreign.Marshal.Alloc
import           Safe
import           System.IO
import           System.ZMQ4


data PipeReaderHandle =
  PipeReaderHandle {
    prThreadId :: ThreadId,
    running    :: IORef Bool
                   } deriving (Eq)


zmqSocketConduit :: (Subscriber a, Receiver a) => T.Text -> Socket a -> IORef Bool -> LogAction IO Message -> Source IO [B.ByteString]
zmqSocketConduit ep sock running' logger = do
  liftIO $ do
    logWith logger Info "PipeReader" $ "Connecting to: " <> ep
    connect sock (T.unpack ep)
    subscribe sock B.empty
  lastHeartbeat <- liftIO $ getCurrentTime >>= newIORef
  whileM_ (andM [notTimeout lastHeartbeat, liftIO (readIORef running')]) $ do
    evs <- liftIO $ poll 200 [Sock sock [In] Nothing]
    unless (null (head evs)) $ do
      bs <- liftIO $ receiveMulti sock
      when ((not . null $ bs) && (head bs == "SYSTEM#HEARTBEAT")) $ liftIO $ getCurrentTime >>= writeIORef lastHeartbeat
      yield bs
  zmqSocketConduit ep sock running' logger
  where
    notTimeout hb = do
      now <- liftIO $ getCurrentTime
      last <- liftIO $ readIORef hb
      return $ now `diffUTCTime` last < 10

parseBarConduit :: Conduit [B.ByteString] IO (TickerId, BarTimeframe, Bar)
parseBarConduit = awaitForever $ \bs ->
  case deserializeBar (BL.fromStrict <$> bs) of
    Just (tf, bar) -> yield (barSecurity bar, tf, bar)
    _              -> return ()

qssdataConduit :: Conduit (TickerId, BarTimeframe, Bar) IO QuoteSourceServerData
qssdataConduit = awaitForever $ \(tid, tf, bar) -> yield $ QSSBar (tf, bar)

chanSink :: (Show a) => BoundedChan a -> Sink a IO ()
chanSink chan = awaitForever
  (\t -> do
    liftIO $ writeChan chan t)

startPipeReader :: Context -> T.Text -> BoundedChan QuoteSourceServerData -> LogAction IO Message -> IO PipeReaderHandle
startPipeReader ctx pipeEndpoint tickChan logger = do
  logWith logger Debug "PipeReader" $ "Trying to open pipe: " <> pipeEndpoint
  s <- socket ctx Sub
  logWith logger Info "PipeReader" "Pipe opened"
  running' <- newIORef True
  tid <- forkIO $ readerThread s running'
  return PipeReaderHandle { prThreadId = tid, running = running' }
    where
      readerThread s running' = runConduit $ (zmqSocketConduit pipeEndpoint s running' logger) =$= parseBarConduit =$= qssdataConduit =$= chanSink tickChan

stopPipeReader :: PipeReaderHandle -> IO ()
stopPipeReader h = killThread (prThreadId h) >> writeIORef (running h) False
