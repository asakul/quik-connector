{-# LANGUAGE OverloadedStrings #-}
module Main where

import QuoteSource.DataImport
import Control.Concurrent hiding (readChan)
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Control.Concurrent.BoundedChan
import ATrade.Types
import QuoteSource.TableParsers.AllParamsTableParser
import QuoteSource.TableParser
import ATrade.QuoteSource.Server

import ATrade.Broker.Server
import ATrade.Broker.Protocol
import Broker.PaperBroker

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

data TableConfig = TableConfig {
  parserId :: String,
  tableName :: String,
  tableParams :: Value
} deriving (Show)

data Config = Config {
  quotesourceEndpoint :: String,
  brokerserverEndpoint :: String,
  tables :: [TableConfig]
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
  return Config { quotesourceEndpoint = qse,
    brokerserverEndpoint = bse,
    tables = rt }
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

forkBoundedChan :: Int -> BoundedChan a -> IO (ThreadId, BoundedChan a, BoundedChan (Maybe a))
forkBoundedChan size source = do
  sink <- newBoundedChan size
  sinkMaybe <- newBoundedChan size
  tid <- forkIO $ forever $ do
    v <- readChan source
    tryWriteChan sink v
    tryWriteChan sinkMaybe (Just v)

  return (tid, sink, sinkMaybe)


main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  infoM "main" "Loading config"
  config <- readConfig "quik-connector.config.json"
  infoM "main" "Config loaded"
  chan <- newBoundedChan 1000
  infoM "main" "Starting data import server"
  dis <- initDataImportServer [MkTableParser $ mkAllParamsTableParser "allparams"] chan "atrade"

  (forkId, c1, c2) <- forkBoundedChan 1000 chan

  broker <- mkPaperBroker c1 1000000 ["demo"]
  withContext (\ctx ->
    bracket (startQuoteSourceServer c2 ctx (T.pack $ quotesourceEndpoint config)) stopQuoteSourceServer (\qsServer ->
      bracket (startBrokerServer [broker] ctx (T.pack $ brokerserverEndpoint config)) stopBrokerServer (\broServer -> do
        void initGUI
        window <- windowNew
        window `on` deleteEvent $ do
          liftIO mainQuit
          return False
        widgetShowAll window
        mainGUI
        infoM "main" "Main thread done")))
  killThread forkId

