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
    prThreadId     :: ThreadId,
    prTickThreadId :: ThreadId,
    running        :: IORef Bool
                   } deriving (Eq)

deserializeTicks :: [B.ByteString] -> [Tick]
deserializeTicks (secname:raw:_) = case decodeUtf8' secname of
  Right tid -> deserializeWithName tid $ BL.fromStrict raw
  Left _    -> []
  where
    deserializeWithName secNameT raw = case deserializeTickBody raw of
      (rest, Just tick) -> tick { security = secNameT } : deserializeWithName secNameT rest
      _ -> []

deserializeTicks _ = []

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

parseTickConduit :: Conduit [B.ByteString] IO ([Tick])
parseTickConduit = awaitForever $ \bs -> do
  yield $ deserializeTicks bs

qssdataConduit :: Conduit (TickerId, BarTimeframe, Bar) IO QuoteSourceServerData
qssdataConduit = awaitForever $ \(tid, tf, bar) -> yield $ QSSBar (tf, bar)

qsstickdataConduit :: Conduit [Tick] IO QuoteSourceServerData
qsstickdataConduit = awaitForever $ \ticks -> forM_ ticks $ \tick -> yield $ QSSTick tick

chanSink :: (Show a) => BoundedChan a -> Sink a IO ()
chanSink chan = awaitForever
  (\t -> do
    liftIO $ writeChan chan t)

startPipeReader :: Context -> T.Text -> T.Text -> BoundedChan QuoteSourceServerData -> LogAction IO Message -> IO PipeReaderHandle
startPipeReader ctx pipeEndpoint tickPipeEndpoint tickChan logger = do
  logWith logger Debug "PipeReader" $ "Trying to open pipe: " <> pipeEndpoint
  s <- socket ctx Sub
  logWith logger Info "PipeReader" "Pipe opened"
  tickSocket <- socket ctx Sub
  logWith logger Info "PipeReader" "Tick pipe opened"
  running' <- newIORef True
  tid <- forkIO $ readerThread s running'
  tid2 <- forkIO $ tickReaderThread tickSocket running'
  return PipeReaderHandle { prThreadId = tid, prTickThreadId = tid2, running = running' }
    where
      readerThread s running' = runConduit $ (zmqSocketConduit pipeEndpoint s running' logger) =$= parseBarConduit =$= qssdataConduit =$= chanSink tickChan
      tickReaderThread s running' = runConduit $
                                      (zmqSocketConduit tickPipeEndpoint s running' logger)
                                  =$= parseTickConduit
                                  =$= qsstickdataConduit
                                  =$= chanSink tickChan

stopPipeReader :: PipeReaderHandle -> IO ()
stopPipeReader h = killThread (prThreadId h) >> killThread (prTickThreadId h) >> writeIORef (running h) False
