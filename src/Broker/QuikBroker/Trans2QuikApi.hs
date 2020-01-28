{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Broker.QuikBroker.Trans2QuikApi (
  Trans2QuikApi(..),
  loadQuikApi,
  Quik(..),
  setCallbacks,
  mkQuik,
  QuikOrder(..),
  QuikTrade(..),
  quikSendTransaction
) where

import           Codec.Text.IConv
import           Control.Concurrent
import           Control.Error.Util
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.IORef
import           Data.Ratio
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Typeable
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           System.Log.Logger
import           System.Win32.DLL
import           System.Win32.Types

type QuikErrorCode = LONG

data QuikException = QuikException T.Text QuikErrorCode
  deriving (Show, Eq, Typeable)

instance Exception QuikException

ecSuccess = 0
ecFailed = 1
ecQuikTerminalNotFound = 2
ecDllVersionNotSupported = 3
ecAlreadyConnectedToQuik = 4
ecWrongSyntax = 5
ecQuikNotConnected = 6
ecDllNotConnected = 7
ecQuikConnected = 8
ecQuikDisconnected = 9
ecDllConnected = 10
ecDllDisconnected = 11
ecMemoryAllocationError = 12
ecWrongConnectionHandle = 13
ecWrongInputParams = 14

type ConnectF = LPCSTR -> Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import stdcall "dynamic"
  mkConnectFun :: FunPtr ConnectF -> ConnectF

type DisconnectF = Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import stdcall "dynamic"
  mkDisconnectFun :: FunPtr DisconnectF -> DisconnectF

type IsQuikConnectedF = Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import stdcall "dynamic"
  mkIsQuikConnectedFun :: FunPtr IsQuikConnectedF -> IsQuikConnectedF

type IsDllConnectedF = Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import stdcall "dynamic"
  mkIsDllConnectedFun :: FunPtr IsDllConnectedF -> IsDllConnectedF

type SendSyncTransactionF = LPSTR -> Ptr LONG -> Ptr LONG -> Ptr CDouble -> LPSTR -> DWORD -> Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import stdcall "dynamic"
  mkSendSyncTransactionFun :: FunPtr SendSyncTransactionF -> SendSyncTransactionF

type SendAsyncTransactionF = LPSTR -> Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import stdcall "dynamic"
  mkSendAsyncTransactionFun :: FunPtr SendAsyncTransactionF -> SendAsyncTransactionF

type ConnectionStatusCallback = LONG -> LONG -> LPSTR -> IO ()
foreign import stdcall "wrapper"
  mkConnectionStatusCallback :: ConnectionStatusCallback -> IO (FunPtr ConnectionStatusCallback)

type SetConnectionStatusCallbackF = FunPtr ConnectionStatusCallback -> Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import stdcall "dynamic"
  mkSetConnectionStatusCallbackFun :: FunPtr SetConnectionStatusCallbackF -> SetConnectionStatusCallbackF

type TransactionsReplyCallback = LONG -> LONG -> LONG -> DWORD -> CLLong -> LPSTR -> CIntPtr -> IO ()
foreign import stdcall "wrapper"
  mkTransactionsReplyCallback :: TransactionsReplyCallback -> IO (FunPtr TransactionsReplyCallback)

type SetTransactionsReplyCallbackF = FunPtr TransactionsReplyCallback -> Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import stdcall "dynamic"
  mkSetTransactionsReplyCallbackFun :: FunPtr SetTransactionsReplyCallbackF -> SetTransactionsReplyCallbackF

type OrderStatusCallback = LONG -> DWORD -> CLLong -> LPSTR -> LPSTR -> CDouble -> CLLong -> CDouble -> LONG -> LONG -> CIntPtr -> IO ()
foreign import stdcall "wrapper"
  mkOrderStatusCallback :: OrderStatusCallback -> IO (FunPtr OrderStatusCallback)

type TradeStatusCallback = LONG -> CLLong -> CLLong -> LPSTR -> LPSTR -> CDouble -> CLLong -> CDouble -> LONG -> CIntPtr -> IO ()
foreign import stdcall "wrapper"
  mkTradeStatusCallback :: TradeStatusCallback -> IO (FunPtr TradeStatusCallback)

type SubscribeOrdersF = LPSTR -> LPSTR -> IO LONG
foreign import stdcall "dynamic"
  mkSubscribeOrdersFun :: FunPtr SubscribeOrdersF -> SubscribeOrdersF

type SubscribeTradesF = LPSTR -> LPSTR -> IO LONG
foreign import stdcall "dynamic"
  mkSubscribeTradesFun :: FunPtr SubscribeTradesF -> SubscribeTradesF

type StartOrdersF = FunPtr OrderStatusCallback -> IO ()
foreign import stdcall "dynamic"
  mkStartOrdersFun :: FunPtr StartOrdersF -> StartOrdersF

type StartTradesF = FunPtr TradeStatusCallback -> IO ()
foreign import stdcall "dynamic"
  mkStartTradesFun :: FunPtr StartTradesF -> StartTradesF

type UnsubscribeOrdersF = IO LONG
foreign import stdcall "dynamic"
  mkUnsubscribeOrdersFun :: FunPtr UnsubscribeOrdersF -> UnsubscribeOrdersF

type UnsubscribeTradesF = IO LONG
foreign import stdcall "dynamic"
  mkUnsubscribeTradesFun :: FunPtr UnsubscribeTradesF -> UnsubscribeTradesF

-- Order requests

type OrderQtyF = LONG -> IO LONG
foreign import stdcall "dynamic"
  mkOrderQtyFun :: FunPtr OrderQtyF -> OrderQtyF

type OrderDateF = LONG -> IO LONG
foreign import stdcall "dynamic"
  mkOrderDateFun :: FunPtr OrderDateF -> OrderDateF

type OrderTimeF = LONG -> IO LONG
foreign import stdcall "dynamic"
  mkOrderTimeFun :: FunPtr OrderTimeF -> OrderTimeF

type OrderActivationTimeF = LONG -> IO LONG
foreign import stdcall "dynamic"
  mkOrderActivationTimeFun :: FunPtr OrderActivationTimeF -> OrderActivationTimeF

type OrderWithdrawTimeF = LONG -> IO LONG
foreign import stdcall "dynamic"
  mkOrderWithdrawTimeFun :: FunPtr OrderWithdrawTimeF -> OrderWithdrawTimeF

type OrderExpiryF = LONG -> IO LONG
foreign import stdcall "dynamic"
  mkOrderExpiryFun :: FunPtr OrderExpiryF -> OrderExpiryF

type OrderAccruedIntF = LONG -> IO CDouble
foreign import stdcall "dynamic"
  mkOrderAccruedIntFun :: FunPtr OrderAccruedIntF -> OrderAccruedIntF

type OrderYieldF = LONG -> IO CDouble
foreign import stdcall "dynamic"
  mkOrderYieldFun :: FunPtr OrderYieldF -> OrderYieldF

type OrderUserIdF = LONG -> IO LPSTR
foreign import stdcall "dynamic"
  mkOrderUserIdFun :: FunPtr OrderUserIdF -> OrderUserIdF

type OrderUidF = LONG -> IO LONG
foreign import stdcall "dynamic"
  mkOrderUidFun :: FunPtr OrderUidF -> OrderUidF

type OrderAccountF = LONG -> IO LPSTR
foreign import stdcall "dynamic"
  mkOrderAccountFun :: FunPtr OrderAccountF -> OrderAccountF

type OrderBrokerRefF = LONG -> IO LPSTR
foreign import stdcall "dynamic"
  mkOrderBrokerRefFun :: FunPtr OrderBrokerRefF -> OrderBrokerRefF

type OrderClientCodeF = LONG -> IO LPSTR
foreign import stdcall "dynamic"
  mkOrderClientCodeFun :: FunPtr OrderClientCodeF -> OrderClientCodeF

type OrderFirmIdF = LONG -> IO LPSTR
foreign import stdcall "dynamic"
  mkOrderFirmIdFun :: FunPtr OrderFirmIdF -> OrderFirmIdF

type OrderVisibleQtyF = LONG -> IO LONG
foreign import stdcall "dynamic"
  mkOrderVisibleQtyFun :: FunPtr OrderVisibleQtyF -> OrderVisibleQtyF

type OrderPeriodF = LONG -> IO LONG
foreign import stdcall "dynamic"
  mkOrderPeriodFun :: FunPtr OrderPeriodF -> OrderPeriodF

type OrderDateTimeF = LONG -> LONG -> IO LONG
foreign import stdcall "dynamic"
  mkOrderDateTimeFun :: FunPtr OrderDateTimeF -> OrderDateTimeF

-- Trade requests

type TradeDateF = CIntPtr -> IO LONG
foreign import stdcall "dynamic"
  mkTradeDateFun :: FunPtr TradeDateF -> TradeDateF

type TradeSettleDateF = CIntPtr -> IO LONG
foreign import stdcall "dynamic"
  mkTradeSettleDateFun :: FunPtr TradeSettleDateF -> TradeSettleDateF

type TradeTimeF = CIntPtr -> IO LONG
foreign import stdcall "dynamic"
  mkTradeTimeFun :: FunPtr TradeTimeF -> TradeTimeF

type TradeIsMarginalF = CIntPtr -> IO LONG
foreign import stdcall "dynamic"
  mkTradeIsMarginalFun :: FunPtr TradeIsMarginalF -> TradeIsMarginalF

type TradeCurrencyF = CIntPtr -> IO LPSTR
foreign import stdcall "dynamic"
  mkTradeCurrencyFun :: FunPtr TradeCurrencyF -> TradeCurrencyF

type TradeSettleCurrencyF = CIntPtr -> IO LPSTR
foreign import stdcall "dynamic"
  mkTradeSettleCurrencyFun :: FunPtr TradeSettleCurrencyF -> TradeSettleCurrencyF

type TradeSettleCodeF = CIntPtr -> IO LPSTR
foreign import stdcall "dynamic"
  mkTradeSettleCodeFun :: FunPtr TradeSettleCodeF -> TradeSettleCodeF

type TradeAccruedIntF = CIntPtr -> IO CDouble
foreign import stdcall "dynamic"
  mkTradeAccruedIntFun :: FunPtr TradeAccruedIntF -> TradeAccruedIntF

type TradeYieldF = CIntPtr -> IO CDouble
foreign import stdcall "dynamic"
  mkTradeYieldFun :: FunPtr TradeYieldF -> TradeYieldF

type TradeUserIdF = CIntPtr -> IO LPSTR
foreign import stdcall "dynamic"
  mkTradeUserIdFun :: FunPtr TradeUserIdF -> TradeUserIdF

type TradeAccountF = CIntPtr -> IO LPSTR
foreign import stdcall "dynamic"
  mkTradeAccountFun :: FunPtr TradeAccountF -> TradeAccountF

type TradeBrokerRefF = CIntPtr -> IO LPSTR
foreign import stdcall "dynamic"
  mkTradeBrokerRefFun :: FunPtr TradeBrokerRefF -> TradeBrokerRefF

type TradeClientCodeF = CIntPtr -> IO LPSTR
foreign import stdcall "dynamic"
  mkTradeClientCodeFun :: FunPtr TradeClientCodeF -> TradeClientCodeF

type TradeFirmIdF = CIntPtr -> IO LPSTR
foreign import stdcall "dynamic"
  mkTradeFirmIdFun :: FunPtr TradeFirmIdF -> TradeFirmIdF

type TradePartnerFirmIdF = CIntPtr -> IO LPSTR
foreign import stdcall "dynamic"
  mkTradePartnerFirmIdFun :: FunPtr TradePartnerFirmIdF -> TradePartnerFirmIdF

type TradeTsCommissionF = CIntPtr -> IO CDouble
foreign import stdcall "dynamic"
  mkTradeTsCommissionFun :: FunPtr TradeTsCommissionF -> TradeTsCommissionF

type TradeClearingCenterCommissionF = CIntPtr -> IO CDouble
foreign import stdcall "dynamic"
  mkTradeClearingCenterCommissionFun :: FunPtr TradeClearingCenterCommissionF -> TradeClearingCenterCommissionF

type TradeExchangeCommissionF = CIntPtr -> IO CDouble
foreign import stdcall "dynamic"
  mkTradeExchangeCommissionFun :: FunPtr TradeExchangeCommissionF -> TradeExchangeCommissionF

type TradeTradingSystemCommissionF = CIntPtr -> IO CDouble
foreign import stdcall "dynamic"
  mkTradeTradingSystemCommissionFun :: FunPtr TradeTradingSystemCommissionF -> TradeTradingSystemCommissionF

type TradePeriodF = CIntPtr -> IO LONG
foreign import stdcall "dynamic"
  mkTradePeriodFun :: FunPtr TradePeriodF -> TradePeriodF

type TradeDateTimeF = CIntPtr -> LONG -> IO LONG
foreign import stdcall "dynamic"
  mkTradeDateTimeFun :: FunPtr TradeDateTimeF -> TradeDateTimeF

type TradeKindF = CIntPtr -> IO LONG
foreign import stdcall "dynamic"
  mkTradeKindFun :: FunPtr TradeKindF -> TradeKindF

toDouble :: CDouble -> Double
toDouble (CDouble x) = x

data Trans2QuikApi = Trans2QuikApi {
  connect                      :: ConnectF,
  disconnect                   :: DisconnectF,
  isQuikConnected              :: IsQuikConnectedF,
  isDllConnected               :: IsDllConnectedF,
  sendSyncTransaction          :: SendSyncTransactionF,
  sendAsyncTransaction         :: SendAsyncTransactionF,
  setConnectionStatusCallback  :: SetConnectionStatusCallbackF,
  setTransactionsReplyCallback :: SetTransactionsReplyCallbackF,
  subscribeOrders              :: SubscribeOrdersF,
  subscribeTrades              :: SubscribeTradesF,
  startOrders                  :: StartOrdersF,
  startTrades                  :: StartTradesF,
  unsubscribeOrders            :: UnsubscribeOrdersF,
  unsubscribeTrades            :: UnsubscribeTradesF,

  orderQty                     :: OrderQtyF,
  orderDate                    :: OrderDateF,
  orderTime                    :: OrderTimeF,
  orderActivationTime          :: OrderActivationTimeF,
  orderWithdrawTime            :: OrderWithdrawTimeF,
  orderExpiry                  :: OrderExpiryF,
  orderAccruedInt              :: OrderAccruedIntF,
  orderYield                   :: OrderYieldF,
  orderUserId                  :: OrderUserIdF,
  orderUid                     :: OrderUidF,
  orderAccount                 :: OrderAccountF,
  orderBrokerRef               :: OrderBrokerRefF,
  orderClientCode              :: OrderClientCodeF,
  orderFirmId                  :: OrderFirmIdF,
  orderVisibleQty              :: OrderVisibleQtyF,
  orderPeriod                  :: OrderPeriodF,
  orderDateTime                :: OrderDateTimeF,

  tradeDate                    :: TradeDateF,
  tradeSettleDate              :: TradeSettleDateF,
  tradeTime                    :: TradeTimeF,
  tradeIsMarginal              :: TradeIsMarginalF,
  tradeCurrency                :: TradeCurrencyF,
  tradeSettleCurrency          :: TradeSettleCurrencyF,
  tradeSettleCode              :: TradeSettleCodeF,
  tradeAccruedInt              :: TradeAccruedIntF,
  tradeYield                   :: TradeYieldF,
  tradeUserId                  :: TradeUserIdF,
  tradeAccount                 :: TradeAccountF,
  tradeBrokerRef               :: TradeBrokerRefF,
  tradeClientCode              :: TradeClientCodeF,
  tradeTsCommission            :: TradeTsCommissionF,
  tradePeriod                  :: TradePeriodF,
  tradeDateTime                :: TradeDateTimeF,
  tradeKind                    :: TradeKindF,

  dllHandle                    :: HMODULE
}

data QuikOrder = QuikOrder {
  qoTransId :: Integer,
  qoOrderId :: Integer,
  qoTicker  :: String,
  qoPrice   :: Double,
  qoBalance :: Integer,
  qoSell    :: Bool,
  qoStatus  :: Int
} deriving (Show, Eq, Ord)

data QuikTrade = QuikTrade {
  qtOrderId        :: Integer,
  qtTicker         :: String,
  qtPrice          :: Double,
  qtQuantity       :: Integer,
  qtSell           :: Bool,
  qtVolume         :: Double,
  qtVolumeCurrency :: String,
  qtTimestamp      :: UTCTime
} deriving (Show, Eq)

  -- Success -> transaction id -> order num -> IO ()
type HlTransactionCallback = Bool -> Integer -> Integer -> IO ()
type HlOrderCallback = QuikOrder -> IO ()
type HlTradeCallback = QuikTrade -> IO ()

data Quik = Quik {
  quikApi               :: Trans2QuikApi,

  connectionCallback    :: FunPtr ConnectionStatusCallback,
  transactionCallback   :: FunPtr TransactionsReplyCallback,
  orderCallback         :: FunPtr OrderStatusCallback,
  tradeCallback         :: FunPtr TradeStatusCallback,

  hlTransactionCallback :: Maybe HlTransactionCallback,
  hlOrderCallback       :: Maybe HlOrderCallback,
  hlTradeCallback       :: Maybe HlTradeCallback,

  connectedToServer     :: Bool,
  connectedToDll        :: Bool,
  watchdogTid           :: ThreadId,

  handledTrades         :: S.Set CLLong,
  handledOrders         :: S.Set QuikOrder
}

quikSendTransaction :: IORef Quik -> String -> IO (Either T.Text ())
quikSendTransaction state transactionString = do
  api <- quikApi <$> readIORef state
  alloca (\errcode ->
    allocaBytes 1024 (\errorMsg ->
      withCString transactionString (\trs -> do
        rc <- sendAsyncTransaction api trs errcode errorMsg 1024
        if rc /= ecSuccess
          then do
            msg <- peekCString errorMsg
            return $ Left $ "Unable to submit transaction: " `T.append` T.pack msg
          else return $ Right ())))


setCallbacks :: IORef Quik -> HlTransactionCallback -> HlOrderCallback -> HlTradeCallback -> IO ()
setCallbacks quik transCb orCb tradeCb = atomicModifyIORef' quik (\s ->
    ( s { hlTransactionCallback = Just transCb,
      hlOrderCallback = Just orCb,
      hlTradeCallback = Just tradeCb }, ()))


mkQuik :: FilePath -> FilePath -> IO (IORef Quik)
mkQuik dllpath quikpath = do
  api <- loadQuikApi dllpath

  debugM "Quik" "Dll loaded"

  myTid <- myThreadId
  state <- newIORef Quik { quikApi = api,
    connectedToServer = False,
    connectedToDll = False,
    watchdogTid = myTid,
    hlTransactionCallback = Nothing,
    hlOrderCallback = Nothing,
    hlTradeCallback = Nothing,
    handledTrades = S.empty,
    handledOrders = S.empty }

  conncb' <- mkConnectionStatusCallback (defaultConnectionCb state)
  transcb' <- mkTransactionsReplyCallback (defaultTransactionReplyCb state)
  orcb' <- mkOrderStatusCallback (defaultOrderCb state)
  tradecb' <- mkTradeStatusCallback (defaultTradeCb state)

  atomicModifyIORef' state (\s -> (s { connectionCallback = conncb',
    transactionCallback = transcb',
    orderCallback = orcb',
    tradeCallback = tradecb' }, ()))

  tid <- forkIO $ watchdog quikpath state
  atomicModifyIORef' state (\s -> (s { watchdogTid = tid }, ()))
  debugM "Quik" "mkQuik done"
  return state

defaultConnectionCb :: IORef Quik -> LONG -> LONG -> LPSTR -> IO ()
defaultConnectionCb state event errorCode infoMessage
  | event == ecQuikConnected = infoM "Quik" "Quik connected" >> atomicModifyIORef' state (\s -> (s { connectedToServer = True }, ()) )
  | event == ecQuikDisconnected = infoM "Quik" "Quik disconnected" >> atomicModifyIORef' state (\s -> (s { connectedToServer = False }, ()) )
  | event == ecDllConnected = infoM "Quik" "DLL connected" >> atomicModifyIORef' state (\s -> (s { connectedToDll = True }, ()) )
  | event == ecDllDisconnected = infoM "Quik" "DLL disconnected" >> atomicModifyIORef' state (\s -> (s { connectedToDll = True }, ()) )
  | otherwise = debugM "Quik" $ "Connection event: " ++ show event

defaultTransactionReplyCb :: IORef Quik -> LONG -> LONG -> LONG -> DWORD -> CLLong -> LPSTR -> CIntPtr -> IO ()
defaultTransactionReplyCb state transactionResult errorCode replyCode transId orderNum replyMessage replyDesc = do
  debugM "Quik" $ "Transaction cb:" ++ show transactionResult ++ "/" ++ show errorCode ++ "/" ++ show replyCode
  when (replyMessage /= nullPtr) $ do
    s <- convert "CP1251" "UTF-8" . BL.fromStrict <$> BS.packCString replyMessage
    case decodeUtf8' (BL.toStrict s) of
      Left _    -> warningM "Quik" "Unable to decode utf-8"
      Right msg -> debugM "Quik" $ "Transaction cb message:" ++ T.unpack msg

  maybecb <- hlTransactionCallback <$> readIORef state
  case maybecb of
    Just cb -> cb ((transactionResult == ecSuccess) && (replyCode /= rcInsufficientFunds)) (toInteger transId) (toInteger orderNum)
    Nothing -> return ()
  where
    rcInsufficientFunds = 4

defaultOrderCb :: IORef Quik -> LONG -> DWORD -> CLLong -> LPSTR -> LPSTR -> CDouble -> CLLong -> CDouble -> LONG -> LONG -> CIntPtr -> IO ()
defaultOrderCb state mode transId dnumber classCode secCode price balance value sell status desc = do
  debugM "Quik" $ "Trade cb: " ++ show mode ++ "/" ++ show dnumber ++ "/" ++ show transId
  orders <- handledOrders <$> readIORef state
  when (mode == 0) $ do
    maybecb <- hlOrderCallback <$> readIORef state
    ssec <- peekCString secCode
    sclass <- peekCString classCode
    let order = mkOrder sclass ssec
    when (order `S.notMember` orders) $ do
      atomicModifyIORef' state (\s -> (s { handledOrders = S.insert order (handledOrders s) }, ()))
      case maybecb of
        Just cb -> cb order
        Nothing -> return ()
      where
        mkOrder :: String -> String -> QuikOrder
        mkOrder sclass ssec = QuikOrder {
          qoTransId = toInteger transId,
          qoOrderId = toInteger dnumber,
          qoTicker = sclass ++ "#" ++ ssec,
          qoPrice = toDouble price,
          qoBalance = toInteger balance,
          qoSell = sell == 1,
          qoStatus = fromIntegral status
        }

defaultTradeCb :: IORef Quik -> LONG -> CLLong -> CLLong -> LPSTR -> LPSTR -> CDouble -> CLLong -> CDouble -> LONG -> CIntPtr -> IO ()
defaultTradeCb state mode dnumber orderNum classCode secCode price qty value sell desc = do
  debugM "Quik" $ "Trade cb: " ++ show mode ++ "/" ++ show dnumber
  trades <- handledTrades <$> readIORef state
  when (mode == 0 && dnumber `S.notMember` trades) $ do
    atomicModifyIORef' state (\s -> (s { handledTrades = S.insert dnumber (handledTrades s) }, ()))
    api <- quikApi <$> readIORef state
    maybecb <- hlTradeCallback <$> readIORef state
    case maybecb of
      Just cb -> do
        ssec <- peekCString secCode
        sclass <- peekCString classCode
        ymd <- toInteger <$> tradeDateTime api desc 0
        hms <- toInteger <$> tradeDateTime api desc 1
        us <- toInteger <$> tradeDateTime api desc 2
        currency <- tradeCurrency api desc >>= peekCString
        cb (trade ssec sclass ymd hms us currency)
      Nothing -> return ()
    where
      trade ssec sclass ymd hms us currency = QuikTrade {
        qtOrderId = toInteger orderNum,
        qtTicker = sclass ++ "#" ++ ssec,
        qtPrice = toDouble price,
        qtQuantity = toInteger qty,
        qtSell = sell == 1,
        qtVolume = toDouble value,
        qtVolumeCurrency = currency,
        qtTimestamp = mkTimestamp ymd hms us
      }
      mkTimestamp ymd hms us = UTCTime (fromGregorian y mon d) (fromInteger (h * 3600 + m * 60 + s) + fromRational (us % 1000000))
        where
          y = ymd `div` 10000
          mon = fromEnum $ (ymd `mod` 10000) `div` 100
          d = fromEnum $ ymd `mod` 100
          h = hms `div` 10000
          m = (hms `mod` 10000) `div` 100
          s = hms `mod` 100


watchdog :: FilePath -> IORef Quik -> IO ()
watchdog quikpath state = do
  api <- quikApi <$> readIORef state
  conncb <- connectionCallback <$> readIORef state
  transcb <- transactionCallback <$> readIORef state
  orcb <- orderCallback <$> readIORef state
  tradecb <- tradeCallback <$> readIORef state

  alloca (\errorCode ->
    allocaBytes 2048 (\errorMsg -> do

      err <- setConnectionStatusCallback api conncb errorCode errorMsg 1024
      if err /= ecSuccess
        then warningM "Quik.Watchdog" $ "Error: " ++ show err
        else forever $ do
          conn <- connectedToDll <$> readIORef state
          handle
            (\(QuikException errmsg rc) -> warningM "Quik.Watchdog" $ (T.unpack errmsg) ++ " (" ++ show rc ++ ")") $
            unless conn $
              withCString quikpath (\path -> do
                err <- connect api path errorCode errorMsg 1024
                if err /= ecSuccess && err /= ecAlreadyConnectedToQuik
                  then warningM "Quik.Watchdog" $ "Unable to connect: " ++ show err
                  else withCString "" (\emptyStr -> do
                    throwIfErr "setTransactionsReplyCallback returned error" $ setTransactionsReplyCallback api transcb errorCode errorMsg 1024
                    throwIfErr "subscribeOrders returned error"  $ subscribeOrders api emptyStr emptyStr
                    startOrders api orcb
                    throwIfErr "subscribeTrades returned error"  $ subscribeTrades api emptyStr emptyStr
                    startTrades api tradecb))
          threadDelay 10000000))

throwIfErr :: T.Text -> IO LONG -> IO ()
throwIfErr errmsg action = do
  rc <- action
  if rc /= ecSuccess
    then throw $ QuikException errmsg rc
    else return ()

loadQuikApi :: FilePath -> IO Trans2QuikApi
loadQuikApi path = do
  dll <- castPtr <$> liftIO (loadLibrary path)
  dll `orFail` "Unable to load Trans2quik.dll"
  connectPtr <- mkConnectFun <$> tryLoad dll "TRANS2QUIK_CONNECT"
  disconnectPtr <- mkDisconnectFun <$> tryLoad dll "TRANS2QUIK_DISCONNECT"
  isQuikConnectedPtr <- mkIsQuikConnectedFun <$> tryLoad dll "TRANS2QUIK_IS_QUIK_CONNECTED"
  isDllConnectedPtr <- mkIsDllConnectedFun <$> tryLoad dll "TRANS2QUIK_IS_DLL_CONNECTED"
  sendSyncTransactionPtr <- mkSendSyncTransactionFun <$> tryLoad dll "TRANS2QUIK_SEND_SYNC_TRANSACTION"
  sendAsyncTransactionPtr <- mkSendAsyncTransactionFun <$> tryLoad dll "TRANS2QUIK_SEND_ASYNC_TRANSACTION"
  setConnectionStatusCallbackPtr <- mkSetConnectionStatusCallbackFun <$> tryLoad dll "TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK"
  setTransactionsReplyCallbackPtr <- mkSetTransactionsReplyCallbackFun <$> tryLoad dll "TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK"
  subscribeOrdersPtr <- mkSubscribeOrdersFun <$> tryLoad dll "TRANS2QUIK_SUBSCRIBE_ORDERS"
  subscribeTradesPtr <- mkSubscribeTradesFun <$> tryLoad dll "TRANS2QUIK_SUBSCRIBE_TRADES"
  startOrdersPtr <- mkStartOrdersFun <$> tryLoad dll "TRANS2QUIK_START_ORDERS"
  startTradesPtr <- mkStartTradesFun <$> tryLoad dll "TRANS2QUIK_START_TRADES"
  unsubscribeOrdersPtr <- mkUnsubscribeOrdersFun <$> tryLoad dll "TRANS2QUIK_UNSUBSCRIBE_ORDERS"
  unsubscribeTradesPtr <- mkUnsubscribeTradesFun <$> tryLoad dll "TRANS2QUIK_UNSUBSCRIBE_TRADES"

  orderQtyPtr <- mkOrderQtyFun <$> tryLoad dll "TRANS2QUIK_ORDER_QTY"
  orderDatePtr <- mkOrderDateFun <$> tryLoad dll "TRANS2QUIK_ORDER_DATE"
  orderTimePtr <- mkOrderTimeFun <$> tryLoad dll "TRANS2QUIK_ORDER_TIME"
  orderActivationTimePtr <- mkOrderActivationTimeFun <$> tryLoad dll "TRANS2QUIK_ORDER_ACTIVATION_TIME"
  orderWithdrawTimePtr <- mkOrderWithdrawTimeFun <$> tryLoad dll "TRANS2QUIK_ORDER_WITHDRAW_TIME"
  orderExpiryPtr <- mkOrderExpiryFun <$> tryLoad dll "TRANS2QUIK_ORDER_EXPIRY"
  orderAccruedIntPtr <- mkOrderAccruedIntFun <$> tryLoad dll "TRANS2QUIK_ORDER_ACCRUED_INT"
  orderYieldPtr <- mkOrderYieldFun <$> tryLoad dll "TRANS2QUIK_ORDER_YIELD"
  orderUserIdPtr <- mkOrderUserIdFun <$> tryLoad dll "TRANS2QUIK_ORDER_USERID"
  orderUidPtr <- mkOrderUidFun <$> tryLoad dll "TRANS2QUIK_ORDER_UID"
  orderAccountPtr <- mkOrderAccountFun <$> tryLoad dll "TRANS2QUIK_ORDER_ACCOUNT"
  orderBrokerRefPtr <- mkOrderBrokerRefFun <$> tryLoad dll "TRANS2QUIK_ORDER_BROKERREF"
  orderClientCodePtr <- mkOrderClientCodeFun <$> tryLoad dll "TRANS2QUIK_ORDER_CLIENT_CODE"
  orderFirmIdPtr <- mkOrderFirmIdFun <$> tryLoad dll "TRANS2QUIK_ORDER_FIRMID"
  orderVisibleQtyPtr <- mkOrderVisibleQtyFun <$> tryLoad dll "TRANS2QUIK_ORDER_VISIBLE_QTY"
  orderPeriodPtr <- mkOrderPeriodFun <$> tryLoad dll "TRANS2QUIK_ORDER_PERIOD"
  orderDateTimePtr <- mkOrderDateTimeFun <$> tryLoad dll "TRANS2QUIK_ORDER_DATE_TIME"

  tradeDatePtr <- mkTradeDateFun <$> tryLoad dll "TRANS2QUIK_TRADE_DATE"
  tradeSettleDatePtr <- mkTradeSettleDateFun <$> tryLoad dll "TRANS2QUIK_TRADE_SETTLE_DATE"
  tradeTimePtr <- mkTradeTimeFun <$> tryLoad dll "TRANS2QUIK_TRADE_TIME"
  tradeIsMarginalPtr <- mkTradeIsMarginalFun <$> tryLoad dll "TRANS2QUIK_TRADE_IS_MARGINAL"
  tradeCurrencyPtr <- mkTradeCurrencyFun <$> tryLoad dll "TRANS2QUIK_TRADE_CURRENCY"
  tradeSettleCurrencyPtr <- mkTradeSettleCurrencyFun <$> tryLoad dll "TRANS2QUIK_TRADE_SETTLE_CURRENCY"
  tradeSettleCodePtr <- mkTradeSettleCodeFun <$> tryLoad dll "TRANS2QUIK_TRADE_SETTLE_CODE"
  tradeAccruedIntPtr <- mkTradeAccruedIntFun <$> tryLoad dll "TRANS2QUIK_TRADE_ACCRUED_INT"
  tradeYieldPtr <- mkTradeYieldFun <$> tryLoad dll "TRANS2QUIK_TRADE_YIELD"
  tradeUserIdPtr <- mkTradeUserIdFun <$> tryLoad dll "TRANS2QUIK_TRADE_USERID"
  tradeAccountPtr <- mkTradeAccountFun <$> tryLoad dll "TRANS2QUIK_TRADE_ACCOUNT"
  tradeBrokerRefPtr <- mkTradeBrokerRefFun <$> tryLoad dll "TRANS2QUIK_TRADE_BROKERREF"
  tradeClientCodePtr <- mkTradeClientCodeFun <$> tryLoad dll "TRANS2QUIK_TRADE_CLIENT_CODE"
  tradeTsCommissionPtr <- mkTradeTsCommissionFun <$> tryLoad dll "TRANS2QUIK_TRADE_TS_COMMISSION"
  tradePeriodPtr <- mkTradePeriodFun <$> tryLoad dll "TRANS2QUIK_TRADE_PERIOD"
  tradeDateTimePtr <- mkTradeDateTimeFun <$> tryLoad dll "TRANS2QUIK_TRADE_DATE_TIME"
  tradeKindPtr <- mkTradeKindFun <$> tryLoad dll "TRANS2QUIK_TRADE_KIND"

  return Trans2QuikApi {
    connect = connectPtr,
    disconnect = disconnectPtr,
    isQuikConnected = isQuikConnectedPtr,
    isDllConnected = isDllConnectedPtr,
    sendSyncTransaction = sendSyncTransactionPtr,
    sendAsyncTransaction = sendAsyncTransactionPtr,
    setConnectionStatusCallback = setConnectionStatusCallbackPtr,
    setTransactionsReplyCallback = setTransactionsReplyCallbackPtr,
    subscribeOrders = subscribeOrdersPtr,
    subscribeTrades = subscribeTradesPtr,
    startOrders = startOrdersPtr,
    startTrades = startTradesPtr,
    unsubscribeOrders = unsubscribeOrdersPtr,
    unsubscribeTrades = unsubscribeTradesPtr,

    orderQty = orderQtyPtr,
    orderDate = orderDatePtr,
    orderTime = orderTimePtr,
    orderActivationTime = orderActivationTimePtr,
    orderWithdrawTime = orderWithdrawTimePtr,
    orderExpiry = orderExpiryPtr,
    orderAccruedInt = orderAccruedIntPtr,
    orderYield = orderYieldPtr,
    orderUserId = orderUserIdPtr,
    orderUid = orderUidPtr,
    orderAccount = orderAccountPtr,
    orderBrokerRef = orderBrokerRefPtr,
    orderClientCode = orderClientCodePtr,
    orderFirmId = orderFirmIdPtr,
    orderVisibleQty = orderVisibleQtyPtr,
    orderPeriod = orderPeriodPtr,
    orderDateTime = orderDateTimePtr,

    tradeDate = tradeDatePtr,
    tradeSettleDate = tradeSettleDatePtr,
    tradeTime = tradeTimePtr,
    tradeIsMarginal = tradeIsMarginalPtr,
    tradeCurrency = tradeCurrencyPtr,
    tradeSettleCurrency = tradeSettleCurrencyPtr,
    tradeSettleCode = tradeSettleCodePtr,
    tradeAccruedInt = tradeAccruedIntPtr,
    tradeYield = tradeYieldPtr,
    tradeUserId = tradeUserIdPtr,
    tradeAccount = tradeAccountPtr,
    tradeBrokerRef = tradeBrokerRefPtr,
    tradeClientCode = tradeClientCodePtr,
    tradeTsCommission = tradeTsCommissionPtr,
    tradePeriod = tradePeriodPtr,
    tradeDateTime = tradeDateTimePtr,
    tradeKind = tradeKindPtr,

    dllHandle = dll
  }

  where
    orFail :: Ptr p -> T.Text -> IO (Ptr p)
    orFail myPtr t = if nullPtr == myPtr
        then throw $ QuikException t ecFailed
        else return myPtr

    tryLoad :: HMODULE -> String -> IO (FunPtr a)
    tryLoad dll proc = do
      p <- getProcAddress' dll proc
      p `orFail` ("Unable to load symbol: " `T.append` T.pack proc)
      return $ castPtrToFunPtr p

    getProcAddress' dll proc = withCAString proc (c_GetProcAddress dll . castPtr)

