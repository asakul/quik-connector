{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE Strict            #-}

module Broker.PaperBroker (
  PaperBrokerState,
  mkPaperBroker
) where

import           ATrade.Broker.Backend
import           ATrade.Broker.Protocol
import           ATrade.Broker.Server
import           ATrade.Logging                 (Message, Severity (..),
                                                 logWith)
import           ATrade.Quotes.QTIS
import           ATrade.Types
import           Colog                          (LogAction)
import           Commissions                    (CommissionConfig (..))
import           Control.Concurrent             hiding (readChan, writeChan)
import           Control.Concurrent.BoundedChan
import           Control.Monad
import           Data.Bits
import           Data.Hashable
import           Data.IORef
import qualified Data.List                      as L
import qualified Data.Map.Strict                as M
import           Data.Maybe
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as TL
import           Data.Time.Clock
import           Language.Haskell.Printf        (t)
import           System.ZMQ4
import           TickTable                      (TickKey (..), TickTableH,
                                                 getTick, getTickerInfo)

data PaperBrokerState = PaperBrokerState {
  pbTid                  :: Maybe ThreadId,
  tickTable              :: TickTableH,
  orders                 :: M.Map OrderId Order,
  cash                   :: !Price,
  notificationCallback   :: Maybe (BrokerBackendNotification -> IO ()),
  pendingOrders          :: [Order],

  fortsClassCodes        :: [T.Text],
  fortsOpenTimeIntervals :: [(DiffTime, DiffTime)],

  auctionableClassCodes  :: [T.Text],
  premarketStartTime     :: DiffTime,
  marketOpenTime         :: DiffTime,
  postMarketStartTime    :: DiffTime,
  postMarketFixTime      :: DiffTime,
  postMarketCloseTime    :: DiffTime,
  commissions            :: [CommissionConfig],
  logger                 :: LogAction IO Message
}

hourMin :: Integer -> Integer -> DiffTime
hourMin h m = fromIntegral $ h * 3600 + m * 60

mkPaperBroker :: TickTableH -> BoundedChan Tick -> Price -> [T.Text] -> [CommissionConfig] -> LogAction IO Message -> IO BrokerBackend
mkPaperBroker tickTableH tickChan startCash accounts comms l = do
  state <- newIORef PaperBrokerState {
    pbTid = Nothing,
    tickTable = tickTableH,
    orders = M.empty,
    cash = startCash,
    notificationCallback = Nothing,
    pendingOrders = [],
    fortsClassCodes = ["SPBFUT", "SPBOPT"],
    fortsOpenTimeIntervals = [(hourMin 7 0, hourMin 11 0), (hourMin 11 5, hourMin 15 45), (hourMin 16 0, hourMin 20 50)],
    auctionableClassCodes = ["TQBR"],
    premarketStartTime = hourMin 6 50,
    marketOpenTime = hourMin 7 0,
    postMarketStartTime = hourMin 15 40,
    postMarketFixTime = hourMin 15 45,
    postMarketCloseTime = hourMin 15 50,
    commissions = comms,
    logger = l
    }

  tid <- forkIO $ brokerThread tickChan state
  atomicModifyIORef' state (\s -> (s { pbTid = Just tid }, ()))

  return BrokerBackend {
    accounts = accounts,
    setNotificationCallback = pbSetNotificationCallback state,
    submitOrder = pbSubmitOrder state,
    cancelOrder = void . pbCancelOrder state,
    stop = pbDestroyBroker state }


brokerThread :: BoundedChan Tick -> IORef PaperBrokerState -> IO ()
brokerThread chan state = forever $ do
    tick <- readChan chan
    marketOpenTime' <- marketOpenTime <$> readIORef state
    when ((utctDayTime . timestamp) tick >= marketOpenTime') $
      executePendingOrders tick state

executePendingOrders tick state = do
  marketOpenTime' <- marketOpenTime <$> readIORef state
  po <- pendingOrders <$> readIORef state
  when (utctDayTime (timestamp tick) >= marketOpenTime') $ do
    executedIds <- catMaybes <$> mapM execute po
    atomicModifyIORef' state (\s -> (s { pendingOrders = L.filter (\order -> orderId order `L.notElem` executedIds) (pendingOrders s)}, ()))
  where
    execute order =
      if security tick == orderSecurity order
        then
          case orderPrice order of
            Market -> do
              log Debug "PaperBroker" "Executing: pending market order"
              executeAtTick state order tick
              return $ Just $ orderId order
            Limit price ->
              executeLimitAt price order
            _ -> return Nothing
        else return Nothing

    log sev comp txt = do
      l <- logger <$> readIORef state
      logWith l sev comp txt


    executeLimitAt price order = case orderOperation order of
      Buy -> if (datatype tick == LastTradePrice && price > value tick && value tick > 0) ||
                (datatype tick == BestOffer && price > value tick && value tick > 0)
        then do
          log Debug "PaperBroker" $ TL.toStrict $ [t|[1]Executing: pending limit order: %Q/%Q|] (security tick) (orderSecurity order)
          executeAtTick state order $ tick { value = price }
          return $ Just $ orderId order
        else return Nothing
      Sell -> if (datatype tick == LastTradePrice && price < value tick && value tick > 0) || (datatype tick == BestBid && price < value tick && value tick > 0)
        then do
          log Debug "PaperBroker" $ TL.toStrict $ [t|[2]Executing: pending limit order: %Q/%Q|] (security tick) (orderSecurity order)
          executeAtTick state order $ tick { value = price }
          return $ Just $ orderId order
        else return Nothing

pbSetNotificationCallback :: IORef PaperBrokerState -> Maybe (BrokerBackendNotification -> IO ()) -> IO()
pbSetNotificationCallback state callback = atomicModifyIORef' state (\s -> (s { notificationCallback = callback }, ()) )

mkTrade :: TickerInfo -> Tick -> Order -> UTCTime -> Maybe CommissionConfig -> Trade
mkTrade info tick order timestamp comconf = Trade {
  tradeOrderId = orderId order,
  tradePrice = value tick,
  tradeQuantity = orderQuantity order,
  tradeVolume = thisTradeVolume,
  tradeVolumeCurrency = "TEST",
  tradeOperation = orderOperation order,
  tradeAccount = orderAccountId order,
  tradeSecurity = orderSecurity order,
  tradeTimestamp = timestamp,
  tradeCommission = 0 `fromMaybe` (calcCommission thisTradeVolume <$> comconf),
  tradeSignalId = orderSignalId order }
  where
    -- Futures have incorrect lotsize
    thisTradeVolume = fromInteger (orderQuantity order) * value tick * if "SPBFUT" `T.isPrefixOf` security tick then 1 else fromInteger (tiLotSize info)
    calcCommission vol c = vol * 0.01 * fromDouble (comPercentage c) + fromDouble (comFixed c) * fromIntegral (orderQuantity order)

maybeCall proj state arg = do
  cb <- proj <$> readIORef state
  case cb of
    Just callback -> callback arg
    Nothing       -> return ()

executeAtTick state order tick = do
  let newOrder = order { orderState = Executed }
  tickerInfo <- obtainTickerInfo (security tick)
  comm <- L.find (\comdef -> comPrefix comdef `T.isPrefixOf` security tick) . commissions <$> readIORef state
  let tradeVolume = fromInteger (orderQuantity order) * value tick * fromInteger (tiLotSize tickerInfo)
  atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s , cash = cash s - tradeVolume}, ()))
  log Debug "PaperBroker" $ TL.toStrict $ [t|Executed: %? at tick: %?|] newOrder tick
  ts <- getCurrentTime
  maybeCall notificationCallback state $ BackendTradeNotification $ mkTrade tickerInfo tick order ts comm
  maybeCall notificationCallback state $ BackendOrderNotification (orderId order) Executed
  where
    obtainTickerInfo tickerId = do
      table <- tickTable <$> readIORef state
      mInfo <- getTickerInfo table tickerId
      case mInfo of
        Just info -> return info
        _ -> return TickerInfo { tiTicker = tickerId,
          tiLotSize = 1,
          tiTickSize = 1 }
    log sev comp txt = do
      l <- logger <$> readIORef state
      logWith l sev comp txt


rejectOrder state order = do
  let newOrder = order { orderState = Rejected } in
            atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s }, ()))
  maybeCall notificationCallback state $ BackendOrderNotification (orderId order) Submitted
  maybeCall notificationCallback state $ BackendOrderNotification (orderId order) Rejected

pbSubmitOrder :: IORef PaperBrokerState -> Order -> IO ()
pbSubmitOrder state order = do
  log Info "PaperBroker" $ "Submitted order: " <> (T.pack . show) order
  case orderPrice order of
    Market             -> executeMarketOrder state order
    Limit price        -> submitLimitOrder price state order
    Stop price trigger -> submitStopOrder state order
    StopMarket trigger -> submitStopMarketOrder state order

  where
    log sev comp txt = do
      l <- logger <$> readIORef state
      logWith l sev comp txt
    executeMarketOrder state order = do
      tm <- tickTable <$> readIORef state
      tickMb <- getTick tm key
      case tickMb of
        Nothing -> rejectOrder state order
        Just tick -> if orderQuantity order /= 0
          then do
            maybeCall notificationCallback state $ BackendOrderNotification (orderId order) Submitted
            executeAtTick state order tick
          else rejectOrder state order
    submitLimitOrder price state order = if orderQuantity order == 0
      then rejectOrder state order
      else do
        tm <- tickTable <$> readIORef state
        tickMb <- getTick tm key
        log Debug "PaperBroker" $ "Limit order submitted, looking up: " <> (T.pack . show) key
        case tickMb of
          Nothing -> do
            let newOrder = order { orderState = Submitted }
            atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s }, ()))
            maybeCall notificationCallback state $ BackendOrderNotification (orderId order) Submitted
          Just tick -> do
            marketOpenTime' <- marketOpenTime <$> readIORef state
            if (((orderOperation order == Buy) && (value tick < price)) ||
                ((orderOperation order == Sell) && (value tick > price)) && (utctDayTime (timestamp tick) >= marketOpenTime'))
              then do
                maybeCall notificationCallback state $ BackendOrderNotification (orderId order) Submitted
                executeAtTick state order tick
              else do
                let newOrder = order { orderState = Submitted }
                atomicModifyIORef' state (\s -> (s { orders = M.insert (orderId order) newOrder $ orders s , pendingOrders = newOrder : pendingOrders s}, ()))
                maybeCall notificationCallback state $ BackendOrderNotification (orderId order) Submitted

    submitStopOrder _ _ = log Warning "PaperBroker" $ "Not implemented: Submitted order: " <> (T.pack . show) order
    submitStopMarketOrder _ _ = log Warning "PaperBroker" $ "Not implemented: Submitted order: " <> (T.pack . show) order

    orderDatatype = case orderOperation order of
      Buy  -> BestOffer
      Sell -> BestBid

    key = TickKey (orderSecurity order) orderDatatype

pbCancelOrder :: IORef PaperBrokerState -> OrderId -> IO Bool
pbCancelOrder state oid = do
  atomicModifyIORef' state (\s -> (s { pendingOrders = L.filter (\o -> orderId o /= oid) (pendingOrders s),
    orders = M.adjustWithKey (\_ v -> v { orderState = Cancelled }) oid (orders s) }, ()))
  maybeCall notificationCallback state $ BackendOrderNotification oid Cancelled
  return True

pbDestroyBroker :: IORef PaperBrokerState -> IO ()
pbDestroyBroker state = do
  maybeTid <- pbTid <$> readIORef state
  case maybeTid of
    Just tid -> killThread tid
    Nothing  -> return ()

