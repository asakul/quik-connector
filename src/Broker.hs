
module Broker (
) where

import Data.Decimal
import Data.Time.Clock

data SignalId = SignalId {
  strategyId :: String,
  signalName :: String,
  comment :: String }
  deriving (Show, Eq)

data OrderPrice = Market | Limit Decimal | Stop Decimal Decimal

data Operation = Buy | Sell
  deriving (Show, Eq)

data OrderState = Unsubmitted
  | Submitted
  | PartiallyExecuted
  | Executed
  | Cancelled
  | Rejected String
  | Error String

data Order = Order {
  orderId :: Integer,
  orderAccountId :: String,
  orderSecurity :: String,
  orderPrice :: OrderPrice,
  orderQuantity :: Integer,
  orderExecutedQuantity :: Integer,
  orderOperation :: Operation,
  orderState :: OrderState,
  orderSignalId :: SignalId }
  deriving (Show, Eq)

data Trade = Trade {
  tradeOrderId :: Integer,
  tradePrice :: Decimal,
  tradeQuantity :: Integer,
  tradeVolume :: Decimal,
  tradeVolumeCurrency :: String,
  tradeAccount :: String,
  tradeSecurity :: String,
  tradeTimestamp :: UTCTime,
  tradeSignalId :: SignalId }
  deriving (Show, Eq)

