{-# LANGUAGE OverloadedStrings #-}

module Broker.Protocol (
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Aeson
import Data.Int
import Broker

type RequestSqnum = Int64

data BrokerServerRequest = RequestSubmitOrder RequestSqnum Order
  | RequestCancelOrder RequestSqnum OrderId
  | RequestNotifications RequestSqnum
  
data BrokerServerResponse = ResponseOrderSubmitted OrderId 
  | ResponseOrderCancelled
  | ResponseNotifications [Notification]

data Notification = OrderNotification OrderId OrderState | TradeNotification Trade

instance FromJSON Notification where
  parseJSON = withObject "notification" (\obj -> do
    tradeJson <- obj .: "trade"
    case tradeJson of
      Just v -> parseTrade v
      Nothing -> do
        orderNotification <- obj .: "order-state"
        case orderNotification of
          Just v -> parseOrder v
          Nothing -> fail "Invalid notification")
    where
      parseTrade v = TradeNotification <$> parseJSON v
      parseOrder (Object o) = case HM.lookup "order-state" o of
        Just v -> withObject "object" (\os -> do
          oid <- os .: "order-id"
          ns <- os .: "new-state"
          return $ OrderNotification oid ns) v
        Nothing -> fail "Should be order-state"

instance ToJSON Notification where
  toJSON (OrderNotification oid 
