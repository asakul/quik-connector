{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Broker.QuikBroker (
  mkQuikBroker
) where

import ATrade.Types
import ATrade.Broker.Protocol
import ATrade.Broker.Server

import Broker.QuikBroker.Trans2QuikApi hiding (tradeAccount)

import Data.Decimal
import Data.IORef
import Data.List.Split
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Bimap as BM
import qualified Data.Text as T

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import System.Log.Logger

import Safe

type QuikOrderId = Integer

data QuikBrokerState = QuikBrokerState {
  notificationCallback :: Maybe (Notification -> IO ()),
  quik :: IORef Quik,
  orderMap :: M.Map OrderId Order,
  orderIdMap :: BM.Bimap QuikOrderId OrderId,
  trans2orderid :: M.Map Integer Order,
  transIdCounter :: Integer
}

nextTransId state = atomicModifyIORef' state (\s -> (s { transIdCounter = transIdCounter s + 1 }, transIdCounter s))

maybeCall proj state arg = do
  cb <- proj <$> readIORef state
  case cb of
    Just callback -> callback arg
    Nothing -> return ()

mkQuikBroker :: FilePath -> FilePath -> [T.Text] -> ExceptT T.Text IO BrokerInterface
mkQuikBroker dllPath quikPath accs = do
  q <- mkQuik dllPath quikPath

  state <- liftIO $ newIORef QuikBrokerState {
    notificationCallback = Nothing,
    quik = q,
    orderMap = M.empty,
    orderIdMap = BM.empty,
    trans2orderid = M.empty,
    transIdCounter = 1
  }

  setCallbacks q (qbTransactionCallback state) (qbOrderCallback state) (qbTradeCallback state)

  return BrokerInterface {
    accounts = accs,
    setNotificationCallback = qbSetNotificationCallback state,
    submitOrder = qbSubmitOrder state,
    cancelOrder = qbCancelOrder state,
    stopBroker = qbStopBroker state
  }

qbSetNotificationCallback state maybecb = atomicModifyIORef' state (\s -> (s {
  notificationCallback = maybecb }, ()))

qbSubmitOrder state order = do
  q <- quik <$> readIORef state
  transId <- nextTransId state
  atomicModifyIORef' state (\s -> (s {
    trans2orderid = M.insert transId order (trans2orderid s) }, ()))
  case makeTransactionString transId order of
    Just transStr -> do
      rc <- quikSendTransaction q transStr
      case rc of
        Left errmsg -> warningM "Quik" $ "Unable to send transaction: " ++ T.unpack errmsg
        Right _ -> debugM "Quik" $ "Order submitted: " ++ show order
    Nothing -> warningM "Quik" $ "Unable to compose transaction string: " ++ show order


qbCancelOrder state orderid = do
  q <- quik <$> readIORef state
  transId <- nextTransId state
  idMap <- orderIdMap <$> readIORef state
  orders <- orderMap <$> readIORef state
  case (BM.lookupR orderid idMap, M.lookup orderid orders) of
    (Just quikOrderId, Just order) -> case makeCancelTransactionString transId order quikOrderId of
      Just transString -> do
        rc <- quikSendTransaction q transString
        case rc of
          Left errmsg -> warningM "Quik" ("Unable to send transaction: " ++ T.unpack errmsg) >> return False
          Right _ -> debugM "Quik" ("Order cancelled: " ++ show orderid) >> return True
      Nothing -> warningM "Quik" ("Unable to compose transaction string: " ++ show orderid) >> return False 
    _ -> warningM "Quik" ("Got request to cancel unknown order: " ++ show orderid) >> return False

qbStopBroker state = return ()

makeTransactionString transId order =
  case (classcode, seccode) of
    (Just cCode, Just sCode) -> Just $
      "ACCOUNT=" ++ T.unpack (orderAccountId order) ++ ";" ++
      "TYPE=" ++ orderTypeCode ++ ";" ++
      "TRANS_ID=" ++ show transId ++ ";" ++
      "CLASSCODE=" ++ cCode ++ ";" ++
      "SECCODE=" ++ sCode ++ ";" ++
      "ACTION=NEW_ORDER;OPERATION=" ++ operationCode ++ ";" ++
      "PRICE=" ++ price ++ ";" ++
      "QUANTITY=" ++ show (orderQuantity order) ++ ";"
    _ -> Nothing
  where
    orderTypeCode = case orderPrice order of
      Market -> "M"
      Limit _ -> "L"
      _ -> "X"
    operationCode = case orderOperation order of
      Buy -> "B"
      Sell -> "S"
    classcode = headMay . splitOn "#" . T.unpack $ orderSecurity order
    seccode = (`atMay` 1) . splitOn "#" . T.unpack $ orderSecurity order
    price = case orderPrice order of
      Market -> "0"
      Limit p -> removeTrailingZeros . show $ p
      _ -> "0"
    removeTrailingZeros v = if '.' `L.elem` v then L.dropWhileEnd (== '.') . L.dropWhileEnd (== '0') $ v else v

makeCancelTransactionString transId order orderId =
  case (classcode, seccode) of
    (Just cCode, Just sCode) -> Just $
      "TRANS_ID=" ++ show transId ++ ";" ++
      "CLASSCODE=" ++ cCode ++ ";" ++
      "SECCODE=" ++ sCode ++ ";" ++
      "ACTION=KILL_ORDER;ORDER_KEY=" ++ show orderId ++ ";"
    _ -> Nothing
  where
    classcode = headMay . splitOn "#" . T.unpack $ orderSecurity order
    seccode = (`atMay` 1) . splitOn "#" . T.unpack $ orderSecurity order

qbTransactionCallback state success transactionId orderNum = do
  t2oid <- trans2orderid <$> readIORef state
  case M.lookup transactionId t2oid of
    Just order -> do
      atomicModifyIORef' state (\s -> (s { trans2orderid = M.delete transactionId t2oid }, ()) )
      newOrder <- if success
        then registerOrder orderNum $ order { orderState = Unsubmitted }
        else registerOrder orderNum $ order { orderState = Rejected }
      maybeCall notificationCallback state (OrderNotification (orderId newOrder) (orderState newOrder))

    Nothing -> return ()
  where
    registerOrder quikOrderId order = atomicModifyIORef' state (\s ->
      (s { orderIdMap = BM.insert quikOrderId (orderId order) (orderIdMap s),
        orderMap = M.insert (orderId order) order (orderMap s) }, order) )

qbOrderCallback state quikorder = do
  orders <- orderMap <$> readIORef state
  idMap <- orderIdMap <$> readIORef state
  debugM "Quik" $ "Order: " ++ show quikorder
  case BM.lookup (qoOrderId quikorder) idMap >>= flip M.lookup orders of
    Just order -> do
      updatedOrder <- if | qoStatus quikorder /= 1 && qoStatus quikorder /= 2 ->
                          if qoBalance quikorder == 0
                            then fullyExecuted order
                            else partiallyExecuted order (orderExecutedQuantity order - qoBalance quikorder)
                         | qoStatus quikorder == 1 ->
                            submitted order
                         | qoStatus quikorder == 2 ->
                            cancelled order
      maybeCall notificationCallback state (OrderNotification (orderId updatedOrder) (orderState updatedOrder))
    Nothing -> warningM "Quik" $ "Unknown order: state callback called: " ++ show quikorder

  where
    updateOrder :: Order -> IO Order
    updateOrder updatedOrder =
      atomicModifyIORef' state (\s -> (s { orderMap = M.insert (orderId updatedOrder) updatedOrder (orderMap s)}, updatedOrder))

    fullyExecuted order = updateOrder $ order { orderState = Executed, orderExecutedQuantity = orderQuantity order }
    partiallyExecuted order quan = updateOrder $ order { orderState = PartiallyExecuted, orderExecutedQuantity = quan }
    submitted order = updateOrder $ order { orderState = Submitted }
    cancelled order = updateOrder $ order { orderState = Cancelled }

qbTradeCallback state quiktrade = do
  orders <- orderMap <$> readIORef state
  idMap <- orderIdMap <$> readIORef state
  debugM "Quik" $ "Trade: " ++ show quiktrade
  case BM.lookup (qtOrderId quiktrade) idMap >>= flip M.lookup orders of
    Just order -> maybeCall notificationCallback state (TradeNotification $ tradeFor order)
    Nothing -> warningM "Quik" $ "Incoming trade for unknown order: " ++ show quiktrade
  where
    tradeFor order = Trade {
      tradeOrderId = orderId order,
      tradePrice = realFracToDecimal 10 $ qtPrice quiktrade,
      tradeQuantity = qtQuantity quiktrade,
      tradeVolume = realFracToDecimal 10 $ qtVolume quiktrade,
      tradeVolumeCurrency = T.pack $ qtVolumeCurrency quiktrade,
      tradeOperation = if qtSell quiktrade then Sell else Buy,
      tradeAccount = orderAccountId order,
      tradeSecurity = orderSecurity order,
      tradeTimestamp = qtTimestamp quiktrade,
      tradeSignalId = orderSignalId order }

