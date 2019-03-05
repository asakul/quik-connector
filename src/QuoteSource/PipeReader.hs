{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module QuoteSource.PipeReader (
  startPipeReader,
  stopPipeReader
  ) where

import           ATrade.QuoteSource.Server
import           ATrade.Types
import           Control.Applicative
import           Control.Concurrent             hiding (readChan, writeChan,
                                                 writeList2Chan, yield)
import           Control.Concurrent.BoundedChan
import           Control.Error.Util
import           Control.Exception
import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Data.Attoparsec.Text
import qualified Data.ByteString                as B
import           Data.Conduit                   hiding (connect)
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.List              as CL
import qualified Data.HashSet                   as HS
import           Data.IORef
import qualified Data.Map.Strict                as M
import           Data.Maybe
import qualified Data.Text                      as T
import           Data.Text.Encoding
import qualified Data.Text.Foreign              as FT
import           Data.Time.Calendar
import           Data.Time.Clock
import           Foreign.Marshal.Alloc
import           Safe
import           System.IO
import           System.Log.Logger              (debugM, warningM)
import           System.ZMQ4


data PipeReaderHandle =
  PipeReaderHandle {
    prThreadId :: ThreadId,
    running    :: IORef Bool
                   } deriving (Eq)

data DataLine = CurrentParamLine T.Text Double Integer Double Integer Integer Double Integer Integer
              | AllTradeLine T.Text Integer Double Integer UTCTime
              deriving (Show, Eq)

yieldJust :: Maybe a -> Source IO a
yieldJust maybeV = do -- Probably already present in some library
  case maybeV of
    Just v  -> yield v
    Nothing -> return ()

zmqSocketConduit :: (Receiver a) => Socket a -> Source IO T.Text
zmqSocketConduit sock = do
  maybeStr <- liftIO $ do
    bs <- receive sock
    case decodeUtf8' bs of
      Left _  -> return Nothing
      Right v -> return (Just v)
  yieldJust maybeStr
  zmqSocketConduit sock

line2TickConduit :: Conduit DataLine IO Tick
line2TickConduit = do
  volumeMap <- liftIO $ newIORef M.empty
  ignoreCPSet <- liftIO $ newIORef HS.empty
  lastTimestamp <- liftIO $ newIORef $ UTCTime (fromGregorian 1970 1 1) 0
  awaitForever $ \line ->
    case line of
      CurrentParamLine tickerId last voltoday bid biddepth biddeptht offer offerdepth offerdeptht -> do
        ts <- liftIO $ readIORef lastTimestamp
        yieldTick tickerId BestBid ts (fromDouble bid) biddepth
        yieldTick tickerId BestOffer ts (fromDouble offer) offerdepth
        yieldTick tickerId TotalSupply ts (fromInteger offerdeptht) 0
        yieldTick tickerId TotalDemand ts (fromInteger biddeptht) 0

        shouldParsePrice <- liftIO $ HS.member tickerId <$> readIORef ignoreCPSet
        when shouldParsePrice $ do
          m <- liftIO $ readIORef volumeMap
          case M.lookup tickerId m of
            Just vol ->
              if | vol < voltoday -> yieldTick tickerId LastTradePrice ts (fromDouble last) (voltoday - vol)
                 | vol > voltoday -> yieldTick tickerId LastTradePrice ts (fromDouble last) vol
                 | otherwise -> return ()
            Nothing -> yieldTick tickerId LastTradePrice ts (fromDouble last) 1

          liftIO $ atomicModifyIORef' volumeMap (\m -> (M.insert tickerId voltoday m, ()))

      AllTradeLine tickerId flags price volume ts -> do
        liftIO $ writeIORef lastTimestamp ts
        if
           | flags == 1 -> yieldTick tickerId LastTradePrice ts (fromDouble price) volume
           | flags == 2 -> yieldTick tickerId LastTradePrice ts (fromDouble price) volume
           | otherwise  -> return ()
        liftIO $ atomicModifyIORef' ignoreCPSet (\s -> (HS.insert tickerId s, ()))

  where
    yieldTick tickerId dtype ts val vol =
      yield $ Tick { security = tickerId,
                     datatype = dtype,
                     timestamp = ts,
                     value = val,
                     volume = vol }

chanSink :: BoundedChan QuoteSourceServerData -> Sink Tick IO ()
chanSink chan = awaitForever
  (\t -> liftIO $ do
      writeChan chan (QSSTick t))

startPipeReader :: Context -> T.Text -> BoundedChan QuoteSourceServerData -> IO PipeReaderHandle
startPipeReader ctx pipeEndpoint tickChan = do
  debugM "PipeReader" $ "Trying to open pipe: " ++ T.unpack pipeEndpoint
  s <- socket ctx Sub
  connect s (T.unpack pipeEndpoint)
  subscribe s B.empty
  debugM "PipeReader" "Pipe opened"
  running' <- newIORef True
  tid <- forkIO $ readerThread s running'
  return PipeReaderHandle { prThreadId = tid, running = running' }
    where
      readerThread s running' = runConduit $ (zmqSocketConduit s) =$= conduitParserEither parseTrade =$= handleParseResult =$= line2TickConduit =$= chanSink tickChan
      parseTrade = parseCurrentParam <|> parseAllTrade
      parseCurrentParam = do
        string "CT:"
        secName <- takeTill (== ':')
        string ":"
        last <- double
        string ";"
        voltoday <- decimal
        string ";"
        bid <- double
        string ";"
        biddepth <- decimal
        string ";"
        biddeptht <- decimal
        string ";"
        offer <- double
        string ";"
        offerdepth <- decimal
        string ";"
        offerdeptht <- decimal
        string ";"
        return $ CurrentParamLine secName last voltoday bid biddepth biddeptht offer offerdepth offerdeptht

      parseAllTrade = do
        string "AT:"
        secName <- takeTill (== ':')
        string ":"
        flags <- decimal
        string ";"
        price <- double
        string ";"
        qty <- decimal
        string ";"
        dt <- parseDateTime
        string ";"
        return $ AllTradeLine secName flags price qty dt

      parseDateTime = do
        y <- decimal
        string "."
        mon <- decimal
        string "."
        day <- decimal
        string " "
        h <- fromInteger <$> decimal
        string ":"
        m <- fromInteger <$> decimal
        string ":"
        s <- fromInteger <$> decimal
        string "."
        ms <- fromInteger <$> decimal
        return $ UTCTime (fromGregorian y mon day) $ h * 3600 + m * 60 + s + ms / 1000

      handleParseResult = do
        awaitForever $ \res ->
          case res of
            Left err -> liftIO $ warningM "PipeReader" $ "Can't parse: " ++ show err
            Right (_, r) -> yield r


stopPipeReader :: PipeReaderHandle -> IO ()
stopPipeReader h = killThread (prThreadId h) >> writeIORef (running h) False
