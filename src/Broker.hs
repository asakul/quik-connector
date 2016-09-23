{-# LANGUAGE OverloadedStrings #-}

module Broker (
  SignalId(..),
  OrderPrice(..),
  Operation(..),
  OrderId(..),
  OrderState(..),
  Order(..),
  Trade(..),
  Broker(..)
) where

import Data.Decimal
import Data.Time.Clock
import Data.Aeson
import Data.Aeson.Types
import Control.Monad

data SignalId = SignalId {
  strategyId :: String,
  signalName :: String,
  comment :: String }
  deriving (Show, Eq)

instance FromJSON SignalId where
  parseJSON (Object o) = SignalId <$>
    o .: "strategy-id" .!= ""     <*>
    o .: "signal-name" .!= ""     <*>
    o .: "commen"      .!= ""
  parseJSON _ = fail "Should be object"

data OrderPrice = Market | Limit Decimal | Stop Decimal Decimal | StopMarket Decimal
  deriving (Show, Eq)

decimal :: (RealFrac r) => r -> Decimal
decimal = realFracToDecimal 10

instance FromJSON OrderPrice where
  parseJSON (String s) = when (s /= "market") (fail "If string, then should be 'market'") >>
    return Market

  parseJSON (Number n) = return $ Limit $ decimal n
  parseJSON (Object v) = do
    triggerPrice <- v .: "trigger" :: Parser Double
    execPrice <- v .: "execution"
    case execPrice of
      (String s) -> when (s /= "market") (fail "If string, then should be 'market'") >> return $ StopMarket (decimal triggerPrice)
      (Number n) -> return $ Stop (decimal triggerPrice) (decimal n)
      _ -> fail "Should be either number or 'market'"

  parseJSON _ = fail "OrderPrice"

data Operation = Buy | Sell
  deriving (Show, Eq)

instance FromJSON Operation where
  parseJSON (String s) 
    | s == "buy" = return Buy
    | s == "sell" = return Sell
    | otherwise = fail "Should be either 'buy' or 'sell'"
  parseJSON _ = fail "Should be string"

type OrderId = Integer

data OrderState = Unsubmitted
  | Submitted
  | PartiallyExecuted
  | Executed
  | Cancelled
  | Rejected String
  | Error String
  deriving (Show, Eq)

instance FromJSON OrderState where
  parseJSON (String s)
    | s == "unsubmitted" = return Unsubmitted
    | s == "submitted" = return Submitted
    | s == "partially-executed" = return PartiallyExecuted
    | s == "executed" = return Executed
    | s == "cancelled" = return Cancelled
    | s == "rejected" = return $ Rejected ""
    | s == "error" = return $ Broker.Error ""
    | otherwise = fail "Invlaid state"

  parseJSON _ = fail "Should be string"

data Order = Order {
  orderId :: OrderId,
  orderAccountId :: String,
  orderSecurity :: String,
  orderPrice :: OrderPrice,
  orderQuantity :: Integer,
  orderExecutedQuantity :: Integer,
  orderOperation :: Operation,
  orderState :: OrderState,
  orderSignalId :: SignalId }
  deriving (Show, Eq)

instance FromJSON Order where
  parseJSON (Object v) = Order      <$>
    v .:? "order-id" .!= 0          <*>
    v .:  "account"                 <*>
    v .:  "security"                <*>
    v .:  "price"                   <*>
    v .:  "quantity"                <*>
    v .:? "executed-quantity" .!= 0 <*>
    v .:  "operation"               <*>
    v .:  "state" .!= Unsubmitted   <*>
    v .:  "signal-id"

  parseJSON _ = fail "Should be string"


data Trade = Trade {
  tradeOrderId :: OrderId,
  tradePrice :: Decimal,
  tradeQuantity :: Integer,
  tradeVolume :: Decimal,
  tradeVolumeCurrency :: String,
  tradeAccount :: String,
  tradeSecurity :: String,
  tradeTimestamp :: UTCTime,
  tradeSignalId :: SignalId }
  deriving (Show, Eq)

data Broker = Broker {
  accounts :: [String],
  setTradeCallback :: Maybe (Trade -> IO ()) -> IO(),
  setOrderCallback :: Maybe (Order -> IO ()) -> IO(),
  submitOrder :: Order -> IO OrderId,
  cancelOrder :: OrderId -> IO (),
  getOrder :: OrderId -> IO (Maybe Order),
  destroyBroker :: IO ()
}

