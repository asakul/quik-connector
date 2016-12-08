{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO

import QuoteSource.DataImport
import Control.Concurrent hiding (readChan)
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Control.Concurrent.BoundedChan
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import ATrade.Types
import QuoteSource.TableParsers.AllParamsTableParser
import QuoteSource.TableParser
import ATrade.QuoteSource.Server

import ATrade.Broker.Server
import ATrade.Broker.Protocol
import Broker.PaperBroker
import Broker.QuikBroker

import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.ZMQ4

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Text as T

import Control.Monad.Trans.Except
import Broker.QuikBroker.Trans2QuikApi

import Network.Telegram

data TableConfig = TableConfig {
  parserId :: String,
  tableName :: String,
  tableParams :: Value
} deriving (Show)

data Config = Config {
  quotesourceEndpoint :: String,
  brokerserverEndpoint :: String,
  tables :: [TableConfig],
  quikPath :: String,
  dllPath :: String,
  quikAccounts :: [T.Text],
  tradeSink :: T.Text,
  telegramToken :: T.Text,
  telegramChatId :: T.Text
} deriving (Show)

readConfig :: String -> IO Config
readConfig fname = do
  content <- BL.readFile fname
  case decode content >>= parseMaybe parseConfig of
    Just config -> return config
    Nothing -> error "Unable to load config"

parseConfig :: Value -> Parser Config
parseConfig = withObject "object" $ \obj -> do
  qse <- obj .: "quotesource-endpoint"
  bse <- obj .: "brokerserver-endpoint"
  rt <- case HM.lookup "tables" obj of
    Just v -> parseTables v
    Nothing -> fail "Expected tables array"
  qp <- obj .: "quik-path"
  dp <- obj .: "dll-path"
  trsink <- obj .: "trade-sink"
  tgToken <- obj .: "telegram-token"
  tgChatId <- obj .: "telegram-chatid"
  accs <- V.toList <$> obj .: "accounts"
  return Config { quotesourceEndpoint = qse,
    brokerserverEndpoint = bse,
    tables = rt,
    quikPath = qp,
    dllPath = dp,
    quikAccounts = fmap T.pack accs,
    tradeSink = trsink,
    telegramToken = tgToken,
    telegramChatId = tgChatId }
  where
    parseTables :: Value -> Parser [TableConfig]
    parseTables = withArray "array" $ \arr -> mapM parseTableConfig (V.toList arr)

    parseTableConfig :: Value -> Parser TableConfig
    parseTableConfig = withObject "object" $ \obj -> do
      pid <- obj .: "parser-id"
      tn <- obj .: "table-name"
      params <- case HM.lookup "params" obj of
        Just x -> return x
        Nothing -> return $ Object HM.empty
      return TableConfig {
        parserId = pid,
        tableName = tn,
        tableParams = params }

forkBoundedChan :: Int -> TBQueue Tick -> IO (ThreadId, TBQueue Tick, TBQueue QuoteSourceServerData)
forkBoundedChan size source = do
  sink <- atomically $ newTBQueue size
  sinkQss <- atomically $ newTBQueue size
  tid <- forkIO $ forever $ do
    v <- atomically $ readTBQueue source
    atomically $ do
      writeTBQueue sink v
      writeTBQueue sinkQss (QSSTick v)

  return (tid, sink, sinkQss)


initLogging = do
  handler <- streamHandler stderr DEBUG >>=
    (\x -> return $
      setFormatter x (simpleLogFormatter "$utcTime\t {$loggername} <$prio> -> $msg"))

  hSetBuffering stderr LineBuffering
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  updateGlobalLogger rootLoggerName (setHandlers [handler])

main :: IO ()
main = do
  initLogging
  infoM "main" "Loading config"
  config <- readConfig "quik-connector.config.json"

  infoM "main" "Config loaded"
  chan <- atomically $ newTBQueue 1000
  infoM "main" "Starting data import server"
  dis <- initDataImportServer [MkTableParser $ mkAllParamsTableParser "allparams"] chan "atrade"

  (forkId, c1, c2) <- forkBoundedChan 1000 chan

  broker <- mkPaperBroker c1 1000000 ["demo"]
  eitherBrokerQ <- runExceptT $ mkQuikBroker (dllPath config) (quikPath config) (quikAccounts config) (Just (telegramToken config, telegramChatId config))
  tgCtx <- mkTelegramContext (telegramToken config)
  sendMessage tgCtx (telegramChatId config) "Goldmine-Quik connector started"
  case eitherBrokerQ of
    Left errmsg -> warningM "main" $ "Can't load quik broker: " ++ T.unpack errmsg
    Right brokerQ ->
      withContext (\ctx ->
        bracket (startQuoteSourceServer c2 ctx (T.pack $ quotesourceEndpoint config)) stopQuoteSourceServer (\qsServer -> do
          bracket (startBrokerServer [broker, brokerQ] ctx (T.pack $ brokerserverEndpoint config) (tradeSink config)) stopBrokerServer (\broServer -> do
            void initGUI
            window <- windowNew
            window `on` deleteEvent $ do
              liftIO mainQuit
              return False
            widgetShowAll window
            mainGUI)
          infoM "main" "BRS down")
        )
  killThread forkId
  infoM "main" "Main thread done"

