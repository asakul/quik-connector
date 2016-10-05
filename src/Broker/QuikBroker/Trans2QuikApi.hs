{-# LANGUAGE OverloadedStrings #-}

module Broker.QuikBroker.Trans2QuikApi (
  Trans2QuikApi(..),
  loadQuikApi
) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Control.Monad.Trans.Except
import Control.Error.Util
import Control.Monad.IO.Class
import System.Win32.DLL
import System.Win32.Types
import qualified Data.Text as T

type ConnectF = LPCSTR -> Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import ccall "dynamic"
  mkConnectFun :: FunPtr ConnectF -> ConnectF

type DisconnectF = Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import ccall "dynamic"
  mkDisconnectFun :: FunPtr DisconnectF -> DisconnectF

type IsQuikConnectedF = Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import ccall "dynamic"
  mkIsQuikConnectedFun :: FunPtr IsQuikConnectedF -> IsQuikConnectedF

type IsDllConnectedF = Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import ccall "dynamic"
  mkIsDllConnectedFun :: FunPtr IsDllConnectedF -> IsDllConnectedF

type SendSyncTransactionF = LPSTR -> Ptr LONG -> Ptr LONG -> Ptr CDouble -> LPSTR -> DWORD -> Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import ccall "dynamic"
  mkSendSyncTransactionFun :: FunPtr SendSyncTransactionF -> SendSyncTransactionF

type SendAsyncTransactionF = LPSTR -> Ptr LONG -> LPSTR -> DWORD -> IO LONG
foreign import ccall "dynamic"
  mkSendAsyncTransactionFun :: FunPtr SendAsyncTransactionF -> SendAsyncTransactionF

type ConnectionStatusCallback = LONG -> LONG -> LPSTR -> IO ()
foreign import ccall "wrapper"
  mkConnectionStatusCallback :: ConnectionStatusCallback -> IO (FunPtr ConnectionStatusCallback)

type SetConnectionStatusCallbackF = FunPtr ConnectionStatusCallback -> Ptr LONG -> LPSTR -> DWORD -> LONG
foreign import ccall "dynamic"
  mkSetConnectionStatusCallbackFun :: FunPtr SetConnectionStatusCallbackF -> SetConnectionStatusCallbackF

type TransactionsReplyCallback = LONG -> LONG -> LONG -> DWORD -> CDouble -> LPSTR -> IO ()
foreign import ccall "wrapper"
  mkTransactionsReplyCallback :: TransactionsReplyCallback -> IO (FunPtr TransactionsReplyCallback)

type SetTransactionsReplyCallbackF = FunPtr TransactionsReplyCallback -> Ptr LONG -> LPSTR -> DWORD -> LONG
foreign import ccall "dynamic"
  mkSetTransactionsReplyCallbackFun :: FunPtr SetTransactionsReplyCallbackF -> SetTransactionsReplyCallbackF

type OrderStatusCallback = LONG -> DWORD -> CDouble -> LPSTR -> LPSTR -> CDouble -> LONG -> CDouble -> LONG -> LONG -> LONG -> IO ()
foreign import ccall "wrapper"
  mkOrderStatusCallback :: OrderStatusCallback -> IO (FunPtr OrderStatusCallback)

type TradeStatusCallback = LONG -> CDouble -> CDouble -> LPSTR -> LPSTR -> CDouble -> LONG -> CDouble -> LONG -> LONG -> IO ()
foreign import ccall "wrapper"
  mkTradeStatusCallback :: TradeStatusCallback -> IO (FunPtr TradeStatusCallback)

type SubscribeOrdersF = LPSTR -> LPSTR -> IO LONG
foreign import ccall "dynamic"
  mkSubscribeOrdersFun :: FunPtr SubscribeOrdersF -> SubscribeOrdersF

type SubscribeTradesF = LPSTR -> LPSTR -> IO LONG
foreign import ccall "dynamic"
  mkSubscribeTradesFun :: FunPtr SubscribeTradesF -> SubscribeTradesF

type StartOrdersF = FunPtr OrderStatusCallback -> IO ()
foreign import ccall "dynamic"
  mkStartOrdersFun :: FunPtr StartOrdersF -> StartOrdersF

type StartTradesF = FunPtr TradeStatusCallback -> IO ()
foreign import ccall "dynamic"
  mkStartTradesFun :: FunPtr StartTradesF -> StartTradesF

type UnsubscribeOrdersF = IO LONG
foreign import ccall "dynamic"
  mkUnsubscribeOrdersFun :: FunPtr UnsubscribeOrdersF -> UnsubscribeOrdersF

type UnsubscribeTradesF = IO LONG
foreign import ccall "dynamic"
  mkUnsubscribeTradesFun :: FunPtr UnsubscribeTradesF -> UnsubscribeTradesF

-- Order requests

type OrderQtyF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkOrderQtyFun :: FunPtr OrderQtyF -> OrderQtyF

type OrderDateF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkOrderDateFun :: FunPtr OrderDateF -> OrderDateF

type OrderTimeF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkOrderTimeFun :: FunPtr OrderTimeF -> OrderTimeF

type OrderActivationTimeF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkOrderActivationTimeFun :: FunPtr OrderActivationTimeF -> OrderActivationTimeF

type OrderWithdrawTimeF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkOrderWithdrawTimeFun :: FunPtr OrderWithdrawTimeF -> OrderWithdrawTimeF

type OrderExpiryF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkOrderExpiryFun :: FunPtr OrderExpiryF -> OrderExpiryF

type OrderAccruedIntF = LONG -> IO CDouble
foreign import ccall "dynamic"
  mkOrderAccruedIntFun :: FunPtr OrderAccruedIntF -> OrderAccruedIntF

type OrderYieldF = LONG -> IO CDouble
foreign import ccall "dynamic"
  mkOrderYieldFun :: FunPtr OrderYieldF -> OrderYieldF

type OrderUserIdF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkOrderUserIdFun :: FunPtr OrderUserIdF -> OrderUserIdF

type OrderUidF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkOrderUidFun :: FunPtr OrderUidF -> OrderUidF

type OrderAccountF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkOrderAccountFun :: FunPtr OrderAccountF -> OrderAccountF

type OrderBrokerRefF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkOrderBrokerRefFun :: FunPtr OrderBrokerRefF -> OrderBrokerRefF

type OrderClientCodeF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkOrderClientCodeFun :: FunPtr OrderClientCodeF -> OrderClientCodeF

type OrderFirmIdF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkOrderFirmIdFun :: FunPtr OrderFirmIdF -> OrderFirmIdF

type OrderVisibleQtyF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkOrderVisibleQtyFun :: FunPtr OrderVisibleQtyF -> OrderVisibleQtyF

type OrderPeriodF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkOrderPeriodFun :: FunPtr OrderPeriodF -> OrderPeriodF

type OrderDateTimeF = LONG -> LONG -> IO LONG
foreign import ccall "dynamic"
  mkOrderDateTimeFun :: FunPtr OrderDateTimeF -> OrderDateTimeF

-- Trade requests

type TradeDateF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkTradeDateFun :: FunPtr TradeDateF -> TradeDateF

type TradeSettleDateF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkTradeSettleDateFun :: FunPtr TradeSettleDateF -> TradeSettleDateF

type TradeTimeF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkTradeTimeFun :: FunPtr TradeTimeF -> TradeTimeF

type TradeIsMarginalF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkTradeIsMarginalFun :: FunPtr TradeIsMarginalF -> TradeIsMarginalF

type TradeCurrencyF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkTradeCurrencyFun :: FunPtr TradeCurrencyF -> TradeCurrencyF

type TradeSettleCurrencyF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkTradeSettleCurrencyFun :: FunPtr TradeSettleCurrencyF -> TradeSettleCurrencyF

type TradeSettleCodeF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkTradeSettleCodeFun :: FunPtr TradeSettleCodeF -> TradeSettleCodeF

type TradeAccruedIntF = LONG -> IO CDouble
foreign import ccall "dynamic"
  mkTradeAccruedIntFun :: FunPtr TradeAccruedIntF -> TradeAccruedIntF

type TradeYieldF = LONG -> IO CDouble
foreign import ccall "dynamic"
  mkTradeYieldFun :: FunPtr TradeYieldF -> TradeYieldF

type TradeUserIdF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkTradeUserIdFun :: FunPtr TradeUserIdF -> TradeUserIdF

type TradeAccountF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkTradeAccountFun :: FunPtr TradeAccountF -> TradeAccountF

type TradeBrokerRefF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkTradeBrokerRefFun :: FunPtr TradeBrokerRefF -> TradeBrokerRefF

type TradeClientCodeF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkTradeClientCodeFun :: FunPtr TradeClientCodeF -> TradeClientCodeF

type TradeFirmIdF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkTradeFirmIdFun :: FunPtr TradeFirmIdF -> TradeFirmIdF

type TradePartnerFirmIdF = LONG -> IO LPSTR
foreign import ccall "dynamic"
  mkTradePartnerFirmIdFun :: FunPtr TradePartnerFirmIdF -> TradePartnerFirmIdF

type TradeTsCommissionF = LONG -> IO CDouble
foreign import ccall "dynamic"
  mkTradeTsCommissionFun :: FunPtr TradeTsCommissionF -> TradeTsCommissionF

type TradeClearingCenterCommissionF = LONG -> IO CDouble
foreign import ccall "dynamic"
  mkTradeClearingCenterCommissionFun :: FunPtr TradeClearingCenterCommissionF -> TradeClearingCenterCommissionF

type TradeExchangeCommissionF = LONG -> IO CDouble
foreign import ccall "dynamic"
  mkTradeExchangeCommissionFun :: FunPtr TradeExchangeCommissionF -> TradeExchangeCommissionF

type TradeTradingSystemCommissionF = LONG -> IO CDouble
foreign import ccall "dynamic"
  mkTradeTradingSystemCommissionFun :: FunPtr TradeTradingSystemCommissionF -> TradeTradingSystemCommissionF

type TradePeriodF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkTradePeriodFun :: FunPtr TradePeriodF -> TradePeriodF

type TradeDateTimeF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkTradeDateTimeFun :: FunPtr TradeDateTimeF -> TradeDateTimeF

type TradeKindF = LONG -> IO LONG
foreign import ccall "dynamic"
  mkTradeKindFun :: FunPtr TradeKindF -> TradeKindF

data Trans2QuikApi = Trans2QuikApi {
  connect :: ConnectF,
  disconnect :: DisconnectF,
  isQuikConnected :: IsQuikConnectedF,
  isDllConnected :: IsDllConnectedF,
  sendSyncTransaction :: SendSyncTransactionF,
  sendAsyncTransaction :: SendAsyncTransactionF,
  setConnectionStatusCallback :: SetConnectionStatusCallbackF,
  setTransactionsReplyCallback :: SetTransactionsReplyCallbackF,
  subscribeOrders :: SubscribeOrdersF,
  subscribeTrades :: SubscribeTradesF,
  startOrders :: StartOrdersF,
  startTrades :: StartTradesF,
  unsubscribeOrders :: UnsubscribeOrdersF,
  unsubscribeTrades :: UnsubscribeTradesF,

  connectionStatusCallback :: Maybe (FunPtr ConnectionStatusCallback),
  transactionReplyCallback :: Maybe (FunPtr TransactionsReplyCallback),
  orderStatusCallback :: Maybe (FunPtr OrderStatusCallback),
  tradeStatusCallback :: Maybe (FunPtr TradeStatusCallback),

  orderQty :: OrderQtyF,
  orderDate :: OrderDateF,
  orderTime :: OrderTimeF,
  orderActivationTime :: OrderActivationTimeF,
  orderWithdrawTime :: OrderWithdrawTimeF,
  orderExpiry :: OrderExpiryF,
  orderAccruedInt :: OrderAccruedIntF,
  orderYield :: OrderYieldF,
  orderUserId :: OrderUserIdF,
  orderUid :: OrderUidF,
  orderAccount :: OrderAccountF,
  orderBrokerRef :: OrderBrokerRefF,
  orderClientCode :: OrderClientCodeF,
  orderFirmId :: OrderFirmIdF,
  orderVisibleQty :: OrderVisibleQtyF,
  orderPeriod :: OrderPeriodF,
  orderDateTime :: OrderDateTimeF,

  tradeDate :: TradeDateF,
  tradeSettleDate :: TradeSettleDateF,
  tradeTime :: TradeTimeF,
  tradeIsMarginal :: TradeIsMarginalF,
  tradeCurrency :: TradeCurrencyF,
  tradeSettleCurrency :: TradeSettleCurrencyF,
  tradeSettleCode :: TradeSettleCodeF,
  tradeAccruedInt :: TradeAccruedIntF,
  tradeYield :: TradeYieldF,
  tradeUserId :: TradeUserIdF,
  tradeAccount :: TradeAccountF,
  tradeBrokerRef :: TradeBrokerRefF,
  tradeClientCode :: TradeClientCodeF,
  tradeTsCommission :: TradeTsCommissionF,
  tradePeriod :: TradePeriodF,
  tradeDateTime :: TradeDateTimeF,
  tradeKind :: TradeKindF,

  dllHandle :: HMODULE
}

loadQuikApi :: FilePath -> ExceptT T.Text IO Trans2QuikApi
loadQuikApi path = do
  dll <- castPtr <$> liftIO (loadLibrary path)
  dll `orFail` "Unable to load Trans2quik.dll"
  connectPtr <- mkConnectFun <$> tryLoad dll "_TRANS2QUIK_CONNECT@16"
  disconnectPtr <- mkDisconnectFun <$> tryLoad dll "_TRANS2QUIK_DISCONNECT@12"
  isQuikConnectedPtr <- mkIsQuikConnectedFun <$> tryLoad dll "_TRANS2QUIK_IS_QUIK_CONNECTED@12"
  isDllConnectedPtr <- mkIsDllConnectedFun <$> tryLoad dll "_TRANS2QUIK_IS_DLL_CONNECTED@12"
  sendSyncTransactionPtr <- mkSendSyncTransactionFun <$> tryLoad dll "_TRANS2QUIK_SEND_SYNC_TRANSACTION@36"
  sendAsyncTransactionPtr <- mkSendAsyncTransactionFun <$> tryLoad dll "_TRANS2QUIK_SEND_ASYNC_TRANSACTION@16"
  setConnectionStatusCallbackPtr <- mkSetConnectionStatusCallbackFun <$> tryLoad dll "_TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK@16"
  setTransactionsReplyCallbackPtr <- mkSetTransactionsReplyCallbackFun <$> tryLoad dll "_TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK@16"
  subscribeOrdersPtr <- mkSubscribeOrdersFun <$> tryLoad dll "_TRANS2QUIK_SUBSCRIBE_ORDERS@8"
  subscribeTradesPtr <- mkSubscribeTradesFun <$> tryLoad dll "_TRANS2QUIK_SUBSCRIBE_TRADES@8"
  startOrdersPtr <- mkStartOrdersFun <$> tryLoad dll "_TRANS2QUIK_START_ORDERS@4"
  startTradesPtr <- mkStartTradesFun <$> tryLoad dll "_TRANS2QUIK_START_TRADES@4"
  unsubscribeOrdersPtr <- mkUnsubscribeOrdersFun <$> tryLoad dll "_TRANS2QUIK_UNSUBSCRIBE_ORDERS@0"
  unsubscribeTradesPtr <- mkUnsubscribeTradesFun <$> tryLoad dll "_TRANS2QUIK_UNSUBSCRIBE_TRADES@0"

  orderQtyPtr <- mkOrderQtyFun <$> tryLoad dll "_TRANS2QUIK_ORDER_QTY@4"
  orderDatePtr <- mkOrderDateFun <$> tryLoad dll "_TRANS2QUIK_ORDER_DATE@4"
  orderTimePtr <- mkOrderTimeFun <$> tryLoad dll "_TRANS2QUIK_ORDER_TIME@4"
  orderActivationTimePtr <- mkOrderActivationTimeFun <$> tryLoad dll "_TRANS2QUIK_ORDER_ACTIVATION_TIME@4"
  orderWithdrawTimePtr <- mkOrderWithdrawTimeFun <$> tryLoad dll "_TRANS2QUIK_ORDER_WITHDRAW_TIME@4"
  orderExpiryPtr <- mkOrderExpiryFun <$> tryLoad dll "_TRANS2QUIK_ORDER_EXPIRY@4"
  orderAccruedIntPtr <- mkOrderAccruedIntFun <$> tryLoad dll "_TRANS2QUIK_ORDER_ACCRUED_INT@4"
  orderYieldPtr <- mkOrderYieldFun <$> tryLoad dll "_TRANS2QUIK_ORDER_YIELD@4"
  orderUserIdPtr <- mkOrderUserIdFun <$> tryLoad dll "_TRANS2QUIK_ORDER_USERID@4"
  orderUidPtr <- mkOrderUidFun <$> tryLoad dll "_TRANS2QUIK_ORDER_UID@4"
  orderAccountPtr <- mkOrderAccountFun <$> tryLoad dll "_TRANS2QUIK_ORDER_ACCOUNT@4"
  orderBrokerRefPtr <- mkOrderBrokerRefFun <$> tryLoad dll "_TRANS2QUIK_ORDER_BROKERREF@4"
  orderClientCodePtr <- mkOrderClientCodeFun <$> tryLoad dll "_TRANS2QUIK_ORDER_CLIENT_CODE@4"
  orderFirmIdPtr <- mkOrderFirmIdFun <$> tryLoad dll "_TRANS2QUIK_ORDER_FIRMID@4"
  orderVisibleQtyPtr <- mkOrderVisibleQtyFun <$> tryLoad dll "_TRANS2QUIK_ORDER_VISIBLE_QTY@4"
  orderPeriodPtr <- mkOrderPeriodFun <$> tryLoad dll "_TRANS2QUIK_ORDER_PERIOD@4"
  orderDateTimePtr <- mkOrderDateTimeFun <$> tryLoad dll "_TRANS2QUIK_ORDER_DATE_TIME@8"

  tradeDatePtr <- mkTradeDateFun <$> tryLoad dll "_TRANS2QUIK_TRADE_DATE@4"
  tradeSettleDatePtr <- mkTradeSettleDateFun <$> tryLoad dll "_TRANS2QUIK_TRADE_SETTLE_DATE@4"
  tradeTimePtr <- mkTradeTimeFun <$> tryLoad dll "_TRANS2QUIK_TRADE_TIME@4"
  tradeIsMarginalPtr <- mkTradeIsMarginalFun <$> tryLoad dll "_TRANS2QUIK_TRADE_IS_MARGINAL@4"
  tradeCurrencyPtr <- mkTradeCurrencyFun <$> tryLoad dll "_TRANS2QUIK_TRADE_CURRENCY@4"
  tradeSettleCurrencyPtr <- mkTradeSettleCurrencyFun <$> tryLoad dll "_TRANS2QUIK_TRADE_SETTLE_CURRENCY@4"
  tradeSettleCodePtr <- mkTradeSettleCodeFun <$> tryLoad dll "_TRANS2QUIK_TRADE_SETTLE_CODE@4"
  tradeAccruedIntPtr <- mkTradeAccruedIntFun <$> tryLoad dll "_TRANS2QUIK_TRADE_ACCRUED_INT@4"
  tradeYieldPtr <- mkTradeYieldFun <$> tryLoad dll "_TRANS2QUIK_TRADE_YIELD@4"
  tradeUserIdPtr <- mkTradeUserIdFun <$> tryLoad dll "_TRANS2QUIK_TRADE_USERID@4"
  tradeAccountPtr <- mkTradeAccountFun <$> tryLoad dll "_TRANS2QUIK_TRADE_ACCOUNT@4"
  tradeBrokerRefPtr <- mkTradeBrokerRefFun <$> tryLoad dll "_TRANS2QUIK_TRADE_BROKERREF@4"
  tradeClientCodePtr <- mkTradeClientCodeFun <$> tryLoad dll "_TRANS2QUIK_TRADE_CLIENT_CODE@4"
  tradeTsCommissionPtr <- mkTradeTsCommissionFun <$> tryLoad dll "_TRANS2QUIK_TRADE_TS_COMMISSION@4"
  tradePeriodPtr <- mkTradePeriodFun <$> tryLoad dll "_TRANS2QUIK_TRADE_PERIOD@4"
  tradeDateTimePtr <- mkTradeDateTimeFun <$> tryLoad dll "_TRANS2QUIK_TRADE_DATE_TIME@8"
  tradeKindPtr <- mkTradeKindFun <$> tryLoad dll "_TRANS2QUIK_TRADE_KIND@4"

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
    orFail :: Ptr p -> T.Text -> ExceptT T.Text IO (Ptr p)
    orFail myPtr t = if nullPtr == myPtr
        then throwE t
        else return myPtr

    tryLoad :: HMODULE -> String -> ExceptT T.Text IO (FunPtr a)
    tryLoad dll proc = do
      p <- liftIO (getProcAddress' dll proc)
      p `orFail` ("Unable to load symbol: " `T.append` T.pack proc)
      return $ castPtrToFunPtr p

    getProcAddress' dll proc = withCAString proc (c_GetProcAddress dll . castPtr)

