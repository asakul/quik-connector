{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

module Broker.PaperBroker (
  PaperBrokerState,
  mkPaperBroker
) where

import Data.Hashable
import Data.Bits
import ATrade.Types
import Data.IORef
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import ATrade.Broker.Protocol
import ATrade.Broker.Server
import Data.Time.Clock
import Data.Maybe
import Control.Monad
import Control.Concurrent.BoundedChan
import Control.Concurrent hiding (readChan, writeChan)
import System.Log.Logger
import ATrade.Quotes.QTIS
import System.ZMQ4

import Commissions (CommissionConfig(..))
import TickTable (TickTableH, TickKey(..), getTick, getTickerInfo)

data PaperBrokerState = PaperBrokerState {
  pbTid :: Maybe ThreadId,
  tickTable :: TickTableH,
  orders :: M.Map OrderId Order,
  cash :: !Price,
  notificationCallback :: Maybe (Notification -> IO ()),
  pendingOrders :: [Order],

  fortsClassCodes :: [T.Text],
  fortsOpenTimeIntervals :: [(DiffTime, DiffTime)],

  auctionableClassCodes :: [T.Text],
  premarketStartTime :: DiffTime,
  marketOpenTime :: DiffTime,
  postMarketStartTime :: DiffTime,
  postMarketFixTime :: DiffTime,
  postMarketCloseTime :: DiffTime,
  commissions :: [CommissionConfig]
}

hourMin :: Integer -> Integer -> DiffTime
hourMin h m = fromIntegral $ h * 3600 + m * 60

mkPaperBroker :: TickTableH -> BoundedChan Tick -> Price -> [T.Text] -> [CommissionConfig] -> IO BrokerInterface
mkPaperBroker tickTableH tickChan startCash accounts comms = do
  state <- newIORef PaperBrokerState {
    pbTid = Nothing,
    tickTable = tickTableH,
    orders = M.empty,
    cash = startCash,
    notificationCallback = Nothing,
    pendingOrders = [],
    fortsClassCodes = ["SPBFUT", "SPBOPT"],
    fortsOpenTimeIntervals = [(hourMin 7 0, hourMin 11 0), (hourMin 11 5, hourMin 15 45), (hourMin 16 0, hourMin 20 50)],
    auctionableClassCodes = ["TQBR"],
    premarketStartTime = hourMin 6 50,
    marketOpenTime = hourMin 7 0,
    postMarketStartTime = hourMin 15 40,
    postMarketFixTime = hourMin 15 45,
    postMarketCloseTime = hourMin 15 50,
    commissions = comms
    }

  tid <- forkIO $ brokerThread tickChan state
  atomicModifyIORef' state (\s -> (s { pbTid = Just tid }, ()))

  return BrokerInterface {
    accounts = accounts,
    setNotificationCallback = pbSetNotificationCallback state,
    submitOrder = pbSubmitOrder state,
    cancelOrder = pbCancelOrder state,
    stopBroker = pbDestroyBroker state }
    
          
brokerThread :: BoundedChan Tick -> IORef PaperBrokerState -> IO ()
brokerThread chan state = forever $ do
    tick <- readChan chan
    marketOpenTime' <- marketOpenTime <$> readIORef state
    when ((utctDayTime . timestamp) tick >= marketOpenTime') $
      executePendingOrders tick state

executePendingOrders tick state = do
  po <- pendingOrders <$> readIORef state
  executedIds <- catMaybes <$> mapM execute po
  atomicModifyIORef' state (\s -> (s { pendingOrders = L.filter (\order -> orderId order `L.notElem` executedIds) (pendingOrders s)}, ()))
  where
    execute order =
      if security tick == orderSecurity order
        then
          case orderPrice order of
            Market -> do
              debugM "PaperBroker" "Executing: pending market order"
              executeAtTick state order tick
              return $ Just $ orderId order
            Limit price -> 
              executeLimitAt price order
            _ -> return Nothing
        else return Nothing


    executeLimitAt price order = case orderOperation order of
      Buy -> if (datatype tick == LastTradePrice && price > value tick && value tick > 0) || (datatype tick == BestOffer && price > value tick && value tick > 0)
        then do
          debugM "PaperBroker" $ "[1]Executing: pending limit order: " ++ show (security tick) ++ "/" ++ show (orderSecurity order)
          executeAtTick state order $ tick { value = price }
          return $ Just $ orderId order
        else return Nothing
      Sell -> if (datatype tick == LastTradePrice && price < value tick && value tick > 0) || (datatype tick == BestBid && price < value tick && value tick > 0)
        then do
          debugM "PaperBroker" $ "[2]Executing: pending limit order: " ++ show (security tick) ++ "/" ++ show (orderSecurity order)
          executeAtTick state order $ tick { value = price }
          return $ Just $ orderId order
        else return Nothing

pbSetNotificationCallback :: IORef PaperBrokerState -> Maybe (Notification -> IO ()) -> IO()
pbSetNotificationCallback state callback = atomicModifyIORef' state (\s -> (s { notificationCallback = callback }, ()) )

mkTrade :: TickerInfo -> Tick -> Order -> UTCTime -> Maybe CommissionConfig -> Trade
mkTrade info tick order timestamp comconf = Trade {
  tradeOrderId = orderId order,
  tradePrice = value tick,
  tradeQuantity = orderQuantity order,
  tradeVolume = thisTradeVolume,
  tradeVolumeCurrency = "TEST",
  tradeOperation = orderOperation order,
  tradeAccount = orderAccountId order,
  tradeSecurity = orderSecurity order,
  tradeTimestamp = timestamp,
  tradeCommission = 0 `fromMaybe` (calcCommission thisTradeVolume <$> comconf),
  tradeSignalId = orderSignalId order }
  where
    -- Futures have incorrect lotsize
    thisTradeVolume = fromInteger (orderQuantity order) * value tick * if "SPBFUT" `T.isPrefixOf` security tick then 1 else fromInteger (tiLotSize info)
    calcCommission vol c = vol * 0.01 * fromDouble (comPercentage c) + fromDouble (comFixed c) * fromIntegral (orderQuantity order)

maybeCall proj state arg = do
  cb <- proj <$> readIORef state
  case cb of
    Just callback -> callback arg
    Nothing -> return ()

executeAtTick state order tick = do
  let newOrder = order { orderState = Executed }
  tickerInfo <- obtainTickerInfo (security tick)
  comm <- L.find (\comdef -> comPrefix comdef `T.isPrefixOf` security tick) . commissions <$> readIORef state
  let tradeVolume = fromInteger (orderQuantity order) * value tick * fromInteger (tiLotSize tickerInfo)
  atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s , cash = cash s - tradeVolume}, ()))
  debugM "PaperBroker" $ "Executed: " ++ show newOrder ++ "; at tick: " ++ show tick
  ts <- getCurrentTime
  maybeCall notificationCallback state $ TradeNotification $ mkTrade tickerInfo tick order ts comm
  maybeCall notificationCallback state $ OrderNotification (orderId order) Executed
  where
    obtainTickerInfo tickerId = do
      table <- tickTable <$> readIORef state
      mInfo <- getTickerInfo table tickerId
      case mInfo of
        Just info -> return info
        _ -> return TickerInfo { tiTicker = tickerId,
          tiLotSize = 1,
          tiTickSize = 1 }

rejectOrder state order = do
  let newOrder = order { orderState = Rejected } in
            atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s }, ()))
  maybeCall notificationCallback state $ OrderNotification (orderId order) Submitted
  maybeCall notificationCallback state $ OrderNotification (orderId order) Rejected

pbSubmitOrder :: IORef PaperBrokerState -> Order -> IO ()
pbSubmitOrder state order = do
  infoM "PaperBroker" $ "Submitted order: " ++ show order
  case orderPrice order of
    Market -> executeMarketOrder state order
    Limit price -> submitLimitOrder price state order
    Stop price trigger -> submitStopOrder state order
    StopMarket trigger -> submitStopMarketOrder state order

  where
    executeMarketOrder state order = do
      tm <- tickTable <$> readIORef state
      tickMb <- getTick tm key
      case tickMb of
        Nothing -> rejectOrder state order
        Just tick -> if orderQuantity order /= 0
          then executeAtTick state order tick
          else rejectOrder state order
    submitLimitOrder price state order = if orderQuantity order == 0
      then rejectOrder state order
      else do
        tm <- tickTable <$> readIORef state
        tickMb <- getTick tm key
        debugM "PaperBroker" $ "Limit order submitted, looking up: " ++ show key
        case tickMb of
          Nothing -> do
            let newOrder = order { orderState = Submitted }
            atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s }, ()))
            maybeCall notificationCallback state $ OrderNotification (orderId order) Submitted
          Just tick ->
            if ((orderOperation order == Buy) && (value tick < price)) || ((orderOperation order == Sell) && (value tick > price))
              then do
                maybeCall notificationCallback state $ OrderNotification (orderId order) Submitted
                executeAtTick state order tick
              else do
                let newOrder = order { orderState = Submitted }
                atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s , pendingOrders = newOrder : pendingOrders s}, ()))
                maybeCall notificationCallback state $ OrderNotification (orderId order) Submitted

    submitStopOrder _ _ = warningM "PaperBroker" $ "Not implemented: Submitted order: " ++ show order
    submitStopMarketOrder _ _ = warningM "PaperBroker" $ "Not implemented: Submitted order: " ++ show order

    orderDatatype = case orderOperation order of
      Buy -> BestOffer
      Sell -> BestBid

    key = TickKey (orderSecurity order) orderDatatype

pbCancelOrder :: IORef PaperBrokerState -> OrderId -> IO Bool
pbCancelOrder state oid = do
  atomicModifyIORef' state (\s -> (s { pendingOrders = L.filter (\o -> orderId o /= oid) (pendingOrders s),
    orders = M.adjustWithKey (\_ v -> v { orderState = Cancelled }) oid (orders s) }, ()))
  maybeCall notificationCallback state $ OrderNotification oid Cancelled
  return True

pbDestroyBroker :: IORef PaperBrokerState -> IO ()
pbDestroyBroker state = do
  maybeTid <- pbTid <$> readIORef state
  case maybeTid of
    Just tid -> killThread tid
    Nothing -> return ()

{-
pbGetOrder :: IORef PaperBrokerState -> OrderId -> IO (Maybe Order)
pbGetOrder state oid = M.lookup oid . orders <$> readIORef state
-}

