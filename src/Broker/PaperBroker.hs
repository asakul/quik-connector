{-# LANGUAGE OverloadedStrings #-}

module Broker.PaperBroker (
  PaperBrokerState,
  mkPaperBroker
) where

import Data.Hashable
import Data.Bits
import Control.Concurrent.BoundedChan
import ATrade.Types
import Data.IORef
import qualified Data.HashMap as M
import qualified Data.Text as T
import ATrade.Broker.Protocol
import ATrade.Broker.Server
import Data.Time.Clock
import Data.Decimal
import Control.Monad
import Control.Concurrent hiding (readChan)
import System.Log.Logger

data TickMapKey = TickMapKey T.Text DataType
  deriving (Show, Eq, Ord)

instance Hashable TickMapKey where
  hashWithSalt salt (TickMapKey s dt) = hashWithSalt salt s `xor` hashWithSalt salt (fromEnum dt)

data PaperBrokerState = PaperBrokerState {
  pbTid :: Maybe ThreadId,
  tickChannel :: BoundedChan Tick,
  tickMap :: M.Map TickMapKey Tick,
  orders :: M.Map OrderId Order,
  cash :: Decimal,
  orderIdCounter :: OrderId,
  notificationCallback :: Maybe (Notification -> IO ())
}

mkPaperBroker :: BoundedChan Tick -> Decimal -> [T.Text] -> IO BrokerInterface
mkPaperBroker tickChan startCash accounts = do
  state <- newIORef PaperBrokerState {
    pbTid = Nothing,
    tickChannel = tickChan,
    tickMap = M.empty,
    orders = M.empty,
    cash = startCash,
    orderIdCounter = 1,
    notificationCallback = Nothing }

  tid <- forkIO $ brokerThread state
  atomicModifyIORef' state (\s -> (s { pbTid = Just tid }, ()) )

  return BrokerInterface {
    accounts = accounts,
    setNotificationCallback = pbSetNotificationCallback state,
    submitOrder = pbSubmitOrder state,
    cancelOrder = pbCancelOrder state,
    stopBroker = pbDestroyBroker state }

brokerThread :: IORef PaperBrokerState -> IO ()
brokerThread state = do
  chan <- tickChannel <$> readIORef state
  forever $ do
    tick <- readChan chan
    atomicModifyIORef' state (\s -> (s { tickMap = M.insert (makeKey tick) tick (tickMap s) }, ()) )
  where
    makeKey tick = TickMapKey (security tick) (datatype tick)

nextOrderId :: IORef PaperBrokerState -> IO OrderId
nextOrderId state = do
  id <- orderIdCounter <$> readIORef state
  modifyIORef state (\s -> s { orderIdCounter = id + 1 } )
  return id

pbSetNotificationCallback :: IORef PaperBrokerState -> Maybe (Notification -> IO ()) -> IO()
pbSetNotificationCallback state callback = modifyIORef state (\s -> s { notificationCallback = callback } )


pbSubmitOrder :: IORef PaperBrokerState -> Order -> IO ()
pbSubmitOrder state order = do
  curState <- readIORef state
  case orderPrice order of
    Market -> executeMarketOrder state order
    Limit price -> submitLimitOrder state order
    Stop price trigger -> submitStopOrder state order
    StopMarket trigger -> submitStopMarketOrder state order

  where
    executeMarketOrder state order = do
      tm <- tickMap <$> readIORef state
      case M.lookup key tm of
        Nothing -> let newOrder = order { orderState = OrderError } in
          atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s }, ()) )

        Just tick -> let newOrder = order { orderState = Executed }
                         tradeVolume = (realFracToDecimal 10 (fromIntegral $ orderQuantity order) * value tick) in do
          atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s , cash = cash s - tradeVolume}, ()) )
          ts <- getCurrentTime
          maybeCall notificationCallback state $ TradeNotification $ mkTrade tick order ts

    submitLimitOrder = undefined
    submitStopOrder = undefined
    submitStopMarketOrder = undefined

    orderDatatype order = case orderOperation order of
      Buy -> BestOffer
      Sell -> BestBid

    key = TickMapKey (orderSecurity order) (orderDatatype order)
    maybeCall proj state arg = do
      cb <- proj <$> readIORef state
      case cb of
        Just callback -> callback arg
        Nothing -> return ()

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


pbCancelOrder :: IORef PaperBrokerState -> OrderId -> IO Bool
pbCancelOrder state order = undefined

pbDestroyBroker :: IORef PaperBrokerState -> IO ()
pbDestroyBroker state = do
  maybeTid <- pbTid <$> readIORef state
  case maybeTid of
    Just tid -> killThread tid
    Nothing -> return ()

pbGetOrder :: IORef PaperBrokerState -> OrderId -> IO (Maybe Order)
pbGetOrder state oid = M.lookup oid . orders <$> readIORef state

