{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

module Broker.PaperBroker (
  PaperBrokerState,
  mkPaperBroker
) where

import Control.DeepSeq
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
import Data.Decimal
import Data.Maybe
import Control.Monad
import Control.Concurrent.BoundedChan
import Control.Concurrent hiding (readChan)
import System.Log.Logger

data TickMapKey = TickMapKey !T.Text !DataType
  deriving (Show, Eq, Ord)

instance Hashable TickMapKey where
  hashWithSalt salt (TickMapKey s dt) = hashWithSalt salt s `xor` hashWithSalt salt (fromEnum dt)

data PaperBrokerState = PaperBrokerState {
  pbTid :: Maybe ThreadId,
  tickMap :: M.Map TickMapKey Tick,
  orders :: M.Map OrderId Order,
  cash :: ! Decimal,
  notificationCallback :: Maybe (Notification -> IO ()),
  pendingOrders :: [Order],

  fortsClassCodes :: [T.Text],
  fortsOpenTimeIntervals :: [(DiffTime, DiffTime)],

  auctionableClassCodes :: [T.Text],
  premarketStartTime :: DiffTime,
  marketOpenTime :: DiffTime,
  postMarketStartTime :: DiffTime,
  postMarketFixTime :: DiffTime,
  postMarketCloseTime :: DiffTime
}

hourMin :: Integer -> Integer -> DiffTime
hourMin h m = fromIntegral $ h * 3600 + m * 60

mkPaperBroker :: BoundedChan Tick -> Decimal -> [T.Text] -> IO BrokerInterface
mkPaperBroker tickChan startCash accounts = do
  state <- newIORef PaperBrokerState {
    pbTid = Nothing,
    tickMap = M.empty,
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
    postMarketCloseTime = hourMin 15 50
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
    atomicModifyIORef' state (\s -> (s { tickMap = M.insert (makeKey tick) tick $! tickMap s }, ()))
    executePendingOrders tick state
  where
    makeKey !tick = TickMapKey (security $! tick) (datatype tick)

executePendingOrders tick state = do
  po <- pendingOrders <$> readIORef state
  executedIds <- catMaybes <$> mapM execute po
  atomicModifyIORef' state (\s -> (s { pendingOrders = L.filter (\order -> orderId order `L.notElem` executedIds) (pendingOrders s)}, ()))
  where
    execute order =
      case orderPrice order of
        Market -> do
          executeAtTick state order tick
          return $ Just $ orderId order
        Limit price -> executeLimitAt price order
        _ -> return Nothing

    executeLimitAt price order = case orderOperation order of
      Buy -> if (datatype tick == Price && price > value tick && value tick > 0) || (datatype tick == BestOffer && price > value tick && value tick > 0)
        then do
          executeAtTick state order $ tick { value = price }
          return $ Just $ orderId order
        else return Nothing
      Sell -> if (datatype tick == Price && price < value tick && value tick > 0) || (datatype tick == BestBid && price < value tick && value tick > 0)
        then do
          executeAtTick state order $ tick { value = price }
          return $ Just $ orderId order
        else return Nothing

pbSetNotificationCallback :: IORef PaperBrokerState -> Maybe (Notification -> IO ()) -> IO()
pbSetNotificationCallback state callback = atomicModifyIORef' state (\s -> (s { notificationCallback = callback }, ()) )

mkTrade :: Tick -> Order -> UTCTime -> Trade
mkTrade tick order timestamp = Trade {
  tradeOrderId = orderId order,
  tradePrice = value tick,
  tradeQuantity = orderQuantity order,
  tradeVolume = realFracToDecimal 10 (fromIntegral $ orderQuantity order) * value tick,
  tradeVolumeCurrency = "TEST",
  tradeOperation = orderOperation order,
  tradeAccount = orderAccountId order,
  tradeSecurity = orderSecurity order,
  tradeTimestamp = timestamp,
  tradeSignalId = orderSignalId order }

maybeCall proj state arg = do
  cb <- proj <$> readIORef state
  case cb of
    Just callback -> callback arg
    Nothing -> return ()

executeAtTick state order tick = do
  let newOrder = order { orderState = Executed }
  let tradeVolume = realFracToDecimal 10 (fromIntegral $ orderQuantity order) * value tick
  atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s , cash = cash s - tradeVolume}, ()))
  debugM "PaperBroker" $ "Executed: " ++ show newOrder
  ts <- getCurrentTime
  maybeCall notificationCallback state $ TradeNotification $ mkTrade tick order ts
  maybeCall notificationCallback state $ OrderNotification (orderId order) Executed

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
      tm <- tickMap <$> readIORef state
      case M.lookup key tm of
        Nothing -> rejectOrder state order
        Just tick -> if orderQuantity order /= 0
          then executeAtTick state order tick
          else rejectOrder state order
    submitLimitOrder price state order = if orderQuantity order == 0
      then rejectOrder state order
      else do
        tm <- tickMap <$> readIORef state
        case M.lookup key tm of
          Nothing -> do
            let newOrder = order { orderState = Submitted }
            atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s }, ()))
            maybeCall notificationCallback state $ OrderNotification (orderId order) Submitted
          Just tick ->
            if ((orderOperation order == Buy) && (value tick < price)) || ((orderOperation order == Sell) && (value tick > price))
              then executeAtTick state order tick
              else do
                let newOrder = order { orderState = Submitted }
                atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s , pendingOrders = newOrder : pendingOrders s}, ()))
                maybeCall notificationCallback state $ OrderNotification (orderId order) Submitted

    submitStopOrder state order = warningM "PaperBroker" $ "Not implemented: Submitted order: " ++ show order
    submitStopMarketOrder state order = warningM "PaperBroker" $ "Not implemented: Submitted order: " ++ show order

    orderDatatype order = case orderOperation order of
      Buy -> BestOffer
      Sell -> BestBid

    key = TickMapKey (orderSecurity order) (orderDatatype order)

pbCancelOrder :: IORef PaperBrokerState -> OrderId -> IO Bool
pbCancelOrder state oid = do
  atomicModifyIORef' state (\s -> (s { pendingOrders = L.filter (\o -> orderId o /= oid) (pendingOrders s),
    orders = M.adjustWithKey (\k v -> v { orderState = Cancelled }) oid (orders s) }, ()))
  maybeCall notificationCallback state $ OrderNotification oid Cancelled
  return True

pbDestroyBroker :: IORef PaperBrokerState -> IO ()
pbDestroyBroker state = do
  maybeTid <- pbTid <$> readIORef state
  case maybeTid of
    Just tid -> killThread tid
    Nothing -> return ()

pbGetOrder :: IORef PaperBrokerState -> OrderId -> IO (Maybe Order)
pbGetOrder state oid = M.lookup oid . orders <$> readIORef state

