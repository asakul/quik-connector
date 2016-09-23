{-# LANGUAGE OverloadedStrings #-}

module Broker.Server (
) where

import System.ZMQ4
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ATrade
import Data.IORef
import qualified Data.HashMap.Strict as HM
import Broker
import Control.Concurrent
import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.Int
import Data.Time.Clock
import Data.List as L
import qualified Data.List.NonEmpty as LN
import System.Log.Logger

type RequestSqnum = Int64
type PeerId = B.ByteString

data BrokerServerState = BrokerServerState {
  bsSocket :: Socket Router,
  orderMap :: M.Map OrderId B.ByteString, -- Matches 0mq client identities with corresponding orders
  lastPacket :: M.Map B.ByteString (RequestSqnum, B.ByteString),
  pendingNotifications :: [(Order, UTCTime)], -- List of tuples (Order with new state, Time when notification enqueued)
  brokers :: [Broker]
}

newtype BrokerServerHandle = BrokerServerHandle ThreadId

mkBrokerServer :: [Broker] -> Context -> String -> IO BrokerServerHandle
mkBrokerServer brokers c ep = do
  sock <- socket c Router
  bind sock ep
  tid <- myThreadId
  state <- newIORef BrokerServerState {
    bsSocket = sock,
    orderMap = M.empty,
    lastPacket = M.empty,
    pendingNotifications = [],
    brokers = brokers
  }
  BrokerServerHandle <$> forkIO (brokerServerThread state)

data BrokerServerMessage = SubmitOrder RequestSqnum Order | CancelOrder RequestSqnum OrderId
data BrokerServerResponse = OrderIdResponse OrderId
instance ToJSON BrokerServerResponse where
  toJSON (OrderIdResponse oid) = object ["order-id" .= oid]

parseMessage :: Value -> Parser BrokerServerMessage
parseMessage (Object obj) = do
  rqsqnum <- obj .: "request-sqnum" :: Parser Int64
  case HM.lookup "order" obj of
    Just (Object orderJson) -> do
      order <- obj .: "order"
      return $ SubmitOrder rqsqnum order
    _ -> case HM.lookup "cancel-order" obj of
      Just orderIdJson -> do
        order <- obj .: "cancel-order"
        return $ CancelOrder rqsqnum order
      Nothing -> fail "Either 'order' or 'cancel-order' field should be present"
  where

parseMessage _ = fail "Should be object"

brokerServerThread :: IORef BrokerServerState -> IO ()
brokerServerThread state = finally brokerServerThread' cleanup
  where
    cleanup = do
      s <- bsSocket <$> readIORef state
      close s

    brokerServerThread' = do
      s <- bsSocket <$> readIORef state
      msg <- receiveMulti s
      tryDeliverPendingNotifications
      handleMessage msg

    tryDeliverPendingNotifications = return ()

    handleMessage :: [B.ByteString] -> IO ()
    handleMessage (peerId:_:json:_) = maybe (return ()) (handleMessage' peerId) (decode (BL.fromStrict json) >>= parseMaybe parseMessage)
    handleMessage _ = warningM "BrokerServer" "Invalid packet received, should be at least 3 parts"

    handleMessage' :: PeerId -> BrokerServerMessage -> IO ()
    handleMessage' peerId (SubmitOrder sqnum order) = do
      s <- bsSocket <$> readIORef state
      lastPack <- M.lookup peerId . lastPacket <$> readIORef state
      case shouldResend lastPack sqnum of
        Just packet -> sendMulti s $ LN.fromList [peerId, B.empty, packet]
        Nothing -> do
          brs <- brokers <$> readIORef state
          case findBroker brs (orderAccountId order) of
            Just broker -> do
              orderId <- submitOrder broker order
              let packet = BL.toStrict . encode $ OrderIdResponse orderId
              atomicModifyIORef' state (\s -> (s { lastPacket = M.insert peerId (sqnum, packet) $ lastPacket s }, ()))
              sendMulti s $ LN.fromList [peerId, B.empty, packet]

            Nothing -> warningM "BrokerServer" $ "Invalid account requested: " ++ orderAccountId order
      where
        shouldResend lastPack sqnum = case lastPack of
          Nothing -> Nothing
          Just (oldSqnum, packet) -> if oldSqnum == sqnum
            then Just packet
            else Nothing
        findBroker brokers account = L.find (L.elem account . accounts) brokers

    handleMessage' peerId (CancelOrder sqnum orderId) = undefined


