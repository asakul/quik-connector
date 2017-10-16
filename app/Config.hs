{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Config (
  TableConfig(..),
  Config(..),
  readConfig

) where 

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
  pipeReaderQsEndpoint :: Maybe String,
  tickPipePath :: Maybe String,
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
  qsePipe <- obj .:? "quotesource-endpoint-pipe-reader"
  pipePath <- obj .:? "pipe-reader-path"
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
    pipeReaderQsEndpoint = qsePipe,
    tickPipePath = pipePath,
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

