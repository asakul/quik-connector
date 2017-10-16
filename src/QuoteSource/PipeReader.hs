{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module QuoteSource.PipeReader (
  startPipeReader,
  stopPipeReader
  ) where

import Data.IORef
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.HashSet as HS
import Data.Time.Clock
import Data.Time.Calendar
import ATrade.Types
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Loops
import Data.Maybe
import Foreign.Marshal.Alloc
import qualified Data.Text.Foreign as FT
import System.Win32.File
import System.Win32.Types
import Control.Concurrent.BoundedChan
import Control.Concurrent hiding (readChan, writeChan, writeList2Chan, yield)
import Control.Exception
import Control.Monad.IO.Class
import Control.Applicative
import Safe
import Control.Error.Util
import Data.Attoparsec.Text
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Attoparsec
import ATrade.QuoteSource.Server

data PipeReaderHandle =
  PipeReaderHandle {
    prThreadId :: ThreadId,
    running :: IORef Bool
                   } deriving (Eq) 

data DataLine = CurrentParamLine T.Text Double Integer Double Integer Integer Double Integer Integer 
              | AllTradeLine T.Text Integer Double Integer UTCTime

yieldJust :: Maybe a -> Source IO a
yieldJust maybeV = do -- Probably already present in some library
  case maybeV of
    Just v -> yield v
    Nothing -> return ()

win32FileConduit :: HANDLE -> Source IO T.Text
win32FileConduit handle = do -- TODO actually i have to check if pipe is closed
  maybeStr <- liftIO $ allocaBytes 4096 $ \buf -> do
    r <- win32_ReadFile handle buf 4096 Nothing
    if r > 0
       then do
         s <- FT.peekCStringLen (buf, fromEnum r)
         return $ Just s
       else return Nothing
  yieldJust maybeStr
  win32FileConduit handle

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
           | flags == 1 -> yieldTick tickerId LastTradePrice ts (fromDouble price) (-volume)
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
chanSink chan = awaitForever (\t -> liftIO $ writeChan chan (QSSTick t))

startPipeReader :: T.Text -> BoundedChan QuoteSourceServerData -> IO PipeReaderHandle
startPipeReader pipeName tickChan = do
  f <- createFile (T.unpack pipeName) gENERIC_READ 0 Nothing oPEN_EXISTING 0 Nothing
  when (f == iNVALID_HANDLE_VALUE) $ error $ "Unable to open pipe: " ++ T.unpack pipeName
  running' <- newIORef True
  tid <- forkIO $ readerThread f running'
  return PipeReaderHandle { prThreadId = tid, running = running' }
    where
      readerThread f running' = runConduit $ (win32FileConduit f) =$= conduitParser parseTrade =$= CL.map snd =$= line2TickConduit =$= chanSink tickChan
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

stopPipeReader :: PipeReaderHandle -> IO ()
stopPipeReader h = killThread (prThreadId h) >> writeIORef (running h) False
