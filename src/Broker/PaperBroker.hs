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
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import ATrade.Types
import Data.IORef
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import ATrade.Broker.Protocol
import ATrade.Broker.Server
import Data.Time.Clock
import Data.Decimal
import Control.Monad
import Control.Concurrent hiding (readChan)
import System.Log.Logger

data TickMapKey = TickMapKey !T.Text !DataType
  deriving (Show, Eq, Ord)

instance Hashable TickMapKey where
  hashWithSalt salt (TickMapKey s dt) = hashWithSalt salt s `xor` hashWithSalt salt (fromEnum dt)

data PaperBrokerState = PaperBrokerState {
  pbTid :: Maybe ThreadId,
  tickMap :: M.HashMap TickMapKey Tick,
  orders :: M.HashMap OrderId Order,
  cash :: ! Decimal,
  notificationCallback :: Maybe (Notification -> IO ())
}

mkPaperBroker :: TBQueue Tick -> Decimal -> [T.Text] -> IO BrokerInterface
mkPaperBroker tickChan startCash accounts = do
  state <- atomically $ newTVar PaperBrokerState {
    pbTid = Nothing,
    tickMap = M.empty,
    orders = M.empty,
    cash = startCash,
    notificationCallback = Nothing }

  tid <- forkIO $ brokerThread tickChan state
  atomically $ modifyTVar' state (\s -> s { pbTid = Just tid })

  return BrokerInterface {
    accounts = accounts,
    setNotificationCallback = pbSetNotificationCallback state,
    submitOrder = pbSubmitOrder state,
    cancelOrder = pbCancelOrder state,
    stopBroker = pbDestroyBroker state }

brokerThread :: TBQueue Tick -> TVar PaperBrokerState -> IO ()
brokerThread chan state = forever $ atomically $ do
    tick <- readTBQueue chan
    modifyTVar' state (\s -> s { tickMap = M.insert (makeKey tick) tick $! tickMap s })
  where
    makeKey !tick = TickMapKey (security $! tick) (datatype tick)

pbSetNotificationCallback :: TVar PaperBrokerState -> Maybe (Notification -> IO ()) -> IO()
pbSetNotificationCallback state callback = atomically $ modifyTVar' state (\s -> s { notificationCallback = callback } )


pbSubmitOrder :: TVar PaperBrokerState -> Order -> IO ()
pbSubmitOrder state order = do
  infoM "PaperBroker" $ "Submitted order: " ++ show order
  case orderPrice order of
    Market -> executeMarketOrder state order
    Limit price -> submitLimitOrder state order
    Stop price trigger -> submitStopOrder state order
    StopMarket trigger -> submitStopMarketOrder state order

  where
    executeMarketOrder state order = do
      tm <- atomically $ tickMap <$> readTVar state
      case M.lookup key tm of
        Nothing -> let newOrder = order { orderState = OrderError } in
          atomically $ modifyTVar' state (\s -> s { orders = M.insert (orderId order) newOrder $ orders s })

        Just tick -> let newOrder = order { orderState = Executed }
                         tradeVolume = (realFracToDecimal 10 (fromIntegral $ orderQuantity order) * value tick) in do
          atomically $ modifyTVar' state (\s -> s { orders = M.insert (orderId order) newOrder $ orders s , cash = cash s - tradeVolume})
          debugM "PaperBroker" $ "Executed: " ++ show newOrder
          ts <- getCurrentTime
          maybeCall notificationCallback state $ TradeNotification $ mkTrade tick order ts
          maybeCall notificationCallback state $ OrderNotification (orderId order) Executed

    submitLimitOrder state order = warningM "PaperBroker" $ "Not implemented: Submitted order: " ++ show order
    submitStopOrder state order = warningM "PaperBroker" $ "Not implemented: Submitted order: " ++ show order
    submitStopMarketOrder state order = warningM "PaperBroker" $ "Not implemented: Submitted order: " ++ show order

    orderDatatype order = case orderOperation order of
      Buy -> BestOffer
      Sell -> BestBid

    key = TickMapKey (orderSecurity order) (orderDatatype order)
    maybeCall proj state arg = do
      cb <- atomically $ proj <$> readTVar state
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


pbCancelOrder :: TVar PaperBrokerState -> OrderId -> IO Bool
pbCancelOrder state order = undefined

pbDestroyBroker :: TVar PaperBrokerState -> IO ()
pbDestroyBroker state = do
  maybeTid <- atomically $ pbTid <$> readTVar state
  case maybeTid of
    Just tid -> killThread tid
    Nothing -> return ()

pbGetOrder :: TVar PaperBrokerState -> OrderId -> IO (Maybe Order)
pbGetOrder state oid = atomically $ M.lookup oid . orders <$> readTVar state

