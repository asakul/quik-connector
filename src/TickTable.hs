{-# LANGUAGE MultiWayIf #-}

module TickTable (
  mkTickTable,
  TickKey(..),
  getTick,
  getTickerInfo,
  TickTableH
) where

import ATrade.Types (DataType(..), TickerId(..), Price(..), Tick(..))

import ATrade.Quotes.QTIS (qtisGetTickersInfo, TickerInfo(..))

import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Concurrent.BoundedChan (BoundedChan, newBoundedChan, readChan, tryReadChan, writeChan)
import Control.Concurrent.MVar (newEmptyMVar)

import Control.Monad (forM_, when, void)

import Data.Maybe (catMaybes, isNothing)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import System.ZMQ4 (Context)

data TickKey = TickKey TickerId DataType
  deriving (Show, Ord, Eq)

data TickTable = TickTable {
  ticks :: M.Map TickKey Tick,
  tickerInfo :: M.Map TickerId TickerInfo
}

type TickTableH = IORef TickTable

data QTISThreadRequest = RequestTickerInfo TickerId | Shutdown

mkTickTable :: BoundedChan Tick -> Context -> T.Text -> IO (IORef TickTable)
mkTickTable chan ctx qtisEndpoint = do
  shutdownMVar <- newEmptyMVar
  qtisChan <- newBoundedChan 10000
  r <- newIORef TickTable { ticks = M.empty,
    tickerInfo = M.empty }
  qtisTid <- forkIO $ qtisThread r qtisChan ctx qtisEndpoint
  void $ forkIO $ tickTableThread qtisChan r shutdownMVar qtisTid
  return r
  where
    tickTableThread qtisChan r shutdownMVar qtisTid = do
      t <- readChan chan
      atomicModifyIORef' r (\s -> (s { ticks = M.insert (TickKey (security t) (datatype t)) t $! ticks s }, ()))
      when (datatype t == LastTradePrice) $ do
        infoMap <- tickerInfo <$> readIORef r
        when (isNothing $ M.lookup (security t) infoMap) $
          writeChan qtisChan $ RequestTickerInfo (security t)

    qtisThread r qtisChan ctx qtisEndpoint = do
      threadDelay 1000000
      requests <- readListFromChan qtisChan
      ti <- qtisGetTickersInfo ctx qtisEndpoint (catMaybes $ fmap requestToTicker requests)
      forM_ ti (\newInfo -> atomicModifyIORef' r (\s -> (s { tickerInfo = M.insert (tiTicker newInfo) newInfo $! tickerInfo s }, ())))

    requestToTicker (RequestTickerInfo t) = Just t
    requestToTicker Shutdown = Nothing

    readListFromChan chan = do
      mh <- tryReadChan chan
      case mh of
        Just h -> do
          t <- readListFromChan' [h] chan
          return $ reverse t
        _ -> do
          h <- readChan chan
          t <- readListFromChan' [h] chan
          return $ reverse t

    readListFromChan' h chan = do
      mv <- tryReadChan chan
      case mv of
        Nothing -> return h
        Just v -> readListFromChan' (v:h) chan

getTick :: TickTableH -> TickKey -> IO (Maybe Tick)
getTick r key = M.lookup key . ticks <$> readIORef r

getTickerInfo :: TickTableH -> TickerId -> IO (Maybe TickerInfo)
getTickerInfo r tickerId = M.lookup tickerId . tickerInfo <$> readIORef r

