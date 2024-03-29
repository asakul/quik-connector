{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Broker.QuikBroker (
  mkQuikBroker
) where

import           ATrade.Broker.Backend
import           ATrade.Broker.Protocol
import           ATrade.Broker.Server
import           ATrade.Quotes.QTIS              (TickerInfo (..))
import           ATrade.Types

import           Broker.QuikBroker.Trans2QuikApi hiding (logger, tradeAccount)

import qualified Data.Bimap                      as BM
import           Data.IORef
import qualified Data.List                       as L
import           Data.List.Split
import qualified Data.Map                        as M
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as TL

import           ATrade.Logging                  (Message, Severity (..),
                                                  logWith)
import           Colog                           (LogAction)
import           Control.Concurrent
import           Control.Concurrent.BoundedChan
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Language.Haskell.Printf         (t)

import           Safe

import           Commissions                     (CommissionConfig (..))
import           TickTable                       (TickKey (..), TickTableH,
                                                  getTick, getTickerInfo)

type QuikOrderId = Integer

data QuikBrokerState = QuikBrokerState {
  notificationCallback :: Maybe (BrokerBackendNotification -> IO ()),
  quik                 :: IORef Quik,
  orderMap             :: M.Map OrderId Order,
  orderIdMap           :: BM.Bimap QuikOrderId OrderId,
  trans2orderid        :: M.Map Integer Order,
  transIdCounter       :: Integer,
  tickTable            :: TickTableH,
  logger               :: LogAction IO Message
}

nextTransId state = atomicModifyIORef' state (\s -> (s { transIdCounter = transIdCounter s + 1 }, transIdCounter s))

maybeCall proj state arg = do
  cb <- proj <$> readIORef state
  case cb of
    Just callback -> callback arg
    Nothing       -> return ()

mkQuikBroker :: TickTableH -> FilePath -> FilePath -> [T.Text] -> [CommissionConfig] -> LogAction IO Message -> IO BrokerBackend
mkQuikBroker tt dllPath quikPath accs comms l = do
  q <- mkQuik dllPath quikPath l

  msgChan <- newBoundedChan 100

  state <- newIORef QuikBrokerState {
    notificationCallback = Nothing,
    quik = q,
    orderMap = M.empty,
    orderIdMap = BM.empty,
    trans2orderid = M.empty,
    transIdCounter = 1,
    tickTable = tt,
    logger = l
  }

  setCallbacks q (qbTransactionCallback state) (qbOrderCallback state) (qbTradeCallback state comms)

  return BrokerBackend {
    accounts = accs,
    setNotificationCallback = qbSetNotificationCallback state,
    submitOrder = qbSubmitOrder state,
    cancelOrder = void . qbCancelOrder state,
    stop = qbStopBroker state
  }

qbSetNotificationCallback state maybecb = atomicModifyIORef' state (\s -> (s {
  notificationCallback = maybecb }, ()))

qbSubmitOrder state order = do
  q <- quik <$> readIORef state
  transId <- nextTransId state
  atomicModifyIORef' state (\s -> (s {
    trans2orderid = M.insert transId order (trans2orderid s) }, ()))
  log Debug "Quik" "Getting ticktable"
  tt <- tickTable <$> readIORef state
  log Debug "Quik" "Getting tickerinfo from ticktable"
  tickerInfoMb <- getTickerInfo tt (orderSecurity order)
  log Debug "Quik" "Getting liquid ticks"
  liquidTickMb <- getTick tt (TickKey (orderSecurity order) (if orderOperation order == Buy then BestOffer else BestBid))
  log Debug "Quik" "Obtained"
  case (tickerInfoMb, liquidTickMb) of
    (Just !tickerInfo, Just !liquidTick) ->
      case makeTransactionString tickerInfo liquidTick transId order of
        Just transStr -> do
          rc <- quikSendTransaction q transStr
          log Debug "Quik" $ "Sending transaction string: " <> T.pack transStr
          case rc of
            Left errmsg -> log Warning "Quik" $ "Unable to send transaction: " <> errmsg
            Right _ -> log Debug "Quik" $ "Order submitted: " <> (T.pack . show) order
        Nothing -> log Warning "Quik" $ "Unable to compose transaction string: " <> (T.pack . show) order
    _ -> log Warning "Quik" $ TL.toStrict $ [t|Unable to obtain data: %?/%?|] tickerInfoMb liquidTickMb
  where
    log sev comp txt = do
      l <- logger <$> readIORef state
      logWith l sev comp txt


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
          Left errmsg -> log Warning "Quik" ("Unable to send transaction: " <> errmsg) >> return False
          Right _ -> log Debug "Quik" ("Order cancelled: " <> (T.pack . show) orderid) >> return True
      Nothing -> log Warning "Quik" ("Unable to compose transaction string: " <> (T.pack . show) orderid) >> return False
    _ -> log Warning "Quik" ("Got request to cancel unknown order: " <> (T.pack . show) orderid) >> return False
  where
    log sev comp txt = do
      l <- logger <$> readIORef state
      logWith l sev comp txt

qbStopBroker state = return ()

makeTransactionString tickerInfo liquidTick transId order =
  case (classcode, seccode, accountTransactionString) of
    (Just cCode, Just sCode, Just accountStr) -> Just $
      accountStr ++
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
      Market  -> "L"
      Limit _ -> "L"
      _       -> "X"
    operationCode = case orderOperation order of
      Buy  -> "B"
      Sell -> "S"
    classcode = headMay . splitOn "#" . T.unpack $ orderSecurity order
    seccode = (`atMay` 1) . splitOn "#" . T.unpack $ orderSecurity order
    price = case orderPrice order of
      Market -> if orderOperation order == Buy
        then removeTrailingZeros . show $ value liquidTick + 10 * tiTickSize tickerInfo
        else removeTrailingZeros . show $ value liquidTick - 10 * tiTickSize tickerInfo
      Limit p -> removeTrailingZeros . show $ p
      _ -> "0"
    removeTrailingZeros v = if '.' `L.elem` v then L.dropWhileEnd (== '.') . L.dropWhileEnd (== '0') $ v else v
    accountTransactionString = case T.splitOn "#" (orderAccountId order) of
      [accountStr, clientCodeStr] -> Just $ "ACCOUNT=" ++ T.unpack accountStr ++ ";CLIENT_CODE=" ++ T.unpack clientCodeStr ++ ";"
      [accountStr] -> Just $ "ACCOUNT=" ++ T.unpack accountStr ++ ";"
      _ -> Nothing

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
      maybeCall notificationCallback state (BackendOrderNotification (orderId newOrder) (orderState newOrder))

    Nothing -> return ()
  where
    registerOrder quikOrderId order = atomicModifyIORef' state (\s ->
      (s { orderIdMap = BM.insert quikOrderId (orderId order) (orderIdMap s),
        orderMap = M.insert (orderId order) order (orderMap s) }, order) )

qbOrderCallback state quikorder = do
  orders <- orderMap <$> readIORef state
  idMap <- orderIdMap <$> readIORef state
  log Debug "Quik" $ "Order: " <> (T.pack . show) quikorder
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
      maybeCall notificationCallback state (BackendOrderNotification (orderId updatedOrder) (orderState updatedOrder))
    Nothing -> log Warning "Quik" $ "Unknown order: state callback called: " <> (T.pack . show) quikorder

  where
    updateOrder :: Order -> IO Order
    updateOrder updatedOrder =
      atomicModifyIORef' state (\s -> (s { orderMap = M.insert (orderId updatedOrder) updatedOrder (orderMap s)}, updatedOrder))

    fullyExecuted order = updateOrder $ order { orderState = Executed, orderExecutedQuantity = orderQuantity order }
    partiallyExecuted order quan = updateOrder $ order { orderState = PartiallyExecuted, orderExecutedQuantity = quan }
    submitted order = updateOrder $ order { orderState = Submitted }
    cancelled order = updateOrder $ order { orderState = Cancelled }

    log sev comp txt = do
      l <- logger <$> readIORef state
      logWith l sev comp txt

qbTradeCallback state comms quiktrade = do
  orders <- orderMap <$> readIORef state
  idMap <- orderIdMap <$> readIORef state
  log Debug "Quik" $ "Trade: " <> (T.pack . show) quiktrade
  case BM.lookup (qtOrderId quiktrade) idMap >>= flip M.lookup orders of
    Just order -> do
      log Debug "Quik" $ "Found comm: " <> (T.pack . show) (L.find (\x -> comPrefix x `T.isPrefixOf` orderSecurity order) comms)
      maybeCall notificationCallback state (BackendTradeNotification $ tradeFor order)
    Nothing -> log Warning "Quik" $ "Incoming trade for unknown order: " <> (T.pack . show) quiktrade
  where
    tradeFor order = Trade {
      tradeOrderId = orderId order,
      tradePrice = fromDouble $ qtPrice quiktrade,
      tradeQuantity = qtQuantity quiktrade,
      tradeVolume = fromDouble $ qtVolume quiktrade,
      tradeVolumeCurrency = T.pack $ qtVolumeCurrency quiktrade,
      tradeOperation = if qtSell quiktrade then Sell else Buy,
      tradeAccount = orderAccountId order,
      tradeSecurity = orderSecurity order,
      tradeTimestamp = qtTimestamp quiktrade,
      tradeCommission = calculateCommission (orderSecurity order) (fromDouble $ qtVolume quiktrade) (qtQuantity quiktrade),
      tradeSignalId = orderSignalId order }

    calculateCommission sec vol qty = case L.find (\x -> comPrefix x `T.isPrefixOf` sec) comms of
      Just com -> vol * fromDouble (0.01 * comPercentage com) + fromDouble (comFixed com) * fromIntegral qty
      Nothing -> 0

    log sev comp txt = do
      l <- logger <$> readIORef state
      logWith l sev comp txt
