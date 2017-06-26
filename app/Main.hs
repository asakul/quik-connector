{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Main where

import System.IO

import QuoteSource.DataImport
import Control.Concurrent hiding (readChan, writeChan)
import Control.Monad
import Control.Exception
import Control.Error.Util
import Control.Monad.IO.Class
import Data.IORef
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Control.Concurrent.BoundedChan
import ATrade.Types
import QuoteSource.TableParsers.AllParamsTableParser
import QuoteSource.TableParser
import ATrade.QuoteSource.Server

import ATrade.Broker.TradeSinks.ZMQTradeSink
import ATrade.Broker.TradeSinks.TelegramTradeSink
import ATrade.Broker.Server
import ATrade.Broker.Protocol
import Broker.PaperBroker
import Broker.QuikBroker

import System.Directory
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.ZMQ4
import System.ZMQ4.ZAP

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Maybe

import Control.Monad.Trans.Except
import Broker.QuikBroker.Trans2QuikApi

import Network.Telegram
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS

data TableConfig = TableConfig {
  parserId :: String,
  tableName :: String,
  tableParams :: Value
} deriving (Show)

data Config = Config {
  quotesourceEndpoint :: String,
  brokerserverEndpoint :: String,
  whitelist :: [T.Text],
  blacklist :: [T.Text],
  brokerServerCertPath :: Maybe FilePath,
  brokerClientCertificateDir :: Maybe FilePath,
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
  whitelist' <- obj .:? "whitelist" .!= []
  blacklist' <- obj .:? "blacklist" .!= []
  serverCert <- obj .:? "broker_server_certificate"
  clientCerts <- obj .:? "broker_client_certificates"
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
    whitelist = whitelist',
    blacklist = blacklist',
    brokerServerCertPath = serverCert,
    brokerClientCertificateDir = clientCerts,
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

forkBoundedChan :: Int -> BoundedChan Tick -> IO (ThreadId, BoundedChan Tick, BoundedChan QuoteSourceServerData)
forkBoundedChan size source = do
  sink <- newBoundedChan size
  sinkQss <- newBoundedChan size
  tid <- forkIO $ forever $ do
      v <- readChan source
      writeChan sink v
      writeChan sinkQss (QSSTick v)

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
  chan <- newBoundedChan 10000
  infoM "main" "Starting data import server"
  dis <- initDataImportServer [MkTableParser $ mkAllParamsTableParser "allparams"] chan "atrade"

  (forkId, c1, c2) <- forkBoundedChan 10000 chan

  broker <- mkPaperBroker c1 1000000 ["demo"]
  man <- newManager (mkManagerSettings (TLSSettingsSimple { settingDisableCertificateValidation = True, settingDisableSession = False, settingUseServerName = False }) Nothing)
  infoM "main" "Http manager created"
  eitherBrokerQ <- runExceptT $ mkQuikBroker man (dllPath config) (quikPath config) (quikAccounts config)
  case eitherBrokerQ of
    Left errmsg -> warningM "main" $ "Can't load quik broker: " ++ T.unpack errmsg
    Right brokerQ ->
      withContext (\ctx ->
        withZapHandler ctx (\zap -> do
          zapSetWhitelist zap $ whitelist config
          zapSetBlacklist zap $ blacklist config

          case brokerClientCertificateDir config of
            Just path -> do
              certs <- loadCertificatesFromDirectory path
              forM_ certs (\cert -> zapAddClientCertificate zap cert)
            Nothing -> return ()

          serverCert <- case brokerServerCertPath config of
            Just certFile -> do
              eitherCert <- loadCertificateFromFile certFile
              case eitherCert of
                Left err -> do
                  warningM "main" $ "Unable to load server certificate: " ++ err
                  return Nothing
                Right cert -> return $ Just cert
            Nothing -> return Nothing
          let serverParams = defaultServerSecurityParams { sspDomain = Just "global",
            sspCertificate = serverCert }

          withZMQTradeSink ctx (tradeSink config) (\zmqTradeSink ->
            withTelegramTradeSink (telegramToken config) (telegramChatId config) (\telegramTradeSink ->
              bracket (startQuoteSourceServer c2 ctx (T.pack $ quotesourceEndpoint config)) stopQuoteSourceServer (\qsServer -> do
                bracket (startBrokerServer [broker, brokerQ] ctx (T.pack $ brokerserverEndpoint config) [telegramTradeSink, zmqTradeSink] serverParams) stopBrokerServer (\broServer -> do
                  Gtk.init Nothing
                  window <- new Gtk.Window [ #title := "Quik connector" ]
                  on window #destroy Gtk.mainQuit
                  #showAll window
                  Gtk.main)
                infoM "main" "BRS down")))))
  killThread forkId
  infoM "main" "Main thread done"

loadCertificatesFromDirectory path = do
  files <- listDirectory path
  catMaybes <$> forM files (\file -> hush <$> loadCertificateFromFile file)

