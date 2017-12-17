{-# LANGUAGE OverloadedStrings, OverloadedLabels, LambdaCase #-}
module Main where

import System.IO

import QuoteSource.DataImport
import Control.Concurrent hiding (readChan, writeChan)
import Control.Monad
import Control.Exception.Safe
import Control.Error.Util
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Control.Concurrent.BoundedChan
import ATrade.Types
import QuoteSource.TableParsers.AllParamsTableParser
import QuoteSource.TableParser
import QuoteSource.PipeReader
import ATrade.QuoteSource.Server

import ATrade.Broker.TradeSinks.ZMQTradeSink
import ATrade.Broker.TradeSinks.TelegramTradeSink
import ATrade.Broker.Server
import Broker.PaperBroker
import Broker.QuikBroker

import System.Directory
import System.Timeout
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.ZMQ4
import System.ZMQ4.ZAP

import qualified Data.Text as T
import Data.Maybe

import Config

forkBoundedChan :: Int -> BoundedChan Tick -> IO (ThreadId, BoundedChan Tick, BoundedChan QuoteSourceServerData)
forkBoundedChan size sourceChan = do
  sink <- newBoundedChan size
  sinkQss <- newBoundedChan size
  tid <- forkIO $ forever $ do
      v <- readChan sourceChan
      writeChan sink v
      writeChan sinkQss (QSSTick v)

  return (tid, sink, sinkQss)


initLogging :: IO ()
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
  _ <- initDataImportServer [MkTableParser $ mkAllParamsTableParser "allparams"] chan "atrade"

  (forkId, c1, c2) <- forkBoundedChan 10000 chan

  brokerQ <- mkQuikBroker (dllPath config) (quikPath config) (quikAccounts config) (commissions config)
  withContext (\ctx -> do
    brokerP <- mkPaperBroker ctx (T.pack $ qtisEndpoint config) c1 1000000 ["demo"] (commissions config)
    withZapHandler ctx (\zap -> do
      zapSetWhitelist zap $ whitelist config
      zapSetBlacklist zap $ blacklist config

      case brokerClientCertificateDir config of
        Just certFile -> do
          certs <- loadCertificatesFromDirectory certFile
          forM_ certs (\cert -> zapAddClientCertificate zap cert)
        Nothing -> return ()

      serverCert <- case brokerServerCertPath config of
        Just certFile -> do
          eitherCert <- loadCertificateFromFile certFile
          case eitherCert of
            Left errorMessage -> do
              warningM "main" $ "Unable to load server certificate: " ++ errorMessage
              return Nothing
            Right cert -> return $ Just cert
        Nothing -> return Nothing
      let serverParams = defaultServerSecurityParams { sspDomain = Just "global",
        sspCertificate = serverCert }

      bracket (forkIO $ pipeReaderThread ctx config) killThread (\_ -> do
        withZMQTradeSink ctx (tradeSink config) (\zmqTradeSink -> do
          withTelegramTradeSink (telegramToken config) (telegramChatId config) (\telegramTradeSink -> do
            bracket (startQuoteSourceServer c2 ctx (T.pack $ quotesourceEndpoint config)) stopQuoteSourceServer (\_ -> do
              bracket (startBrokerServer [brokerP, brokerQ] ctx (T.pack $ brokerserverEndpoint config) [telegramTradeSink, zmqTradeSink] serverParams) stopBrokerServer (\_ -> do
                void $ Gtk.init Nothing
                window <- new Gtk.Window [ #title := "Quik connector" ]
                void $ on window #destroy Gtk.mainQuit
                #showAll window
                Gtk.main)
              infoM "main" "BRS down")
            debugM "main" "QS done")
          debugM "main" "TGTS done")
        debugM "main" "ZMQTS done")
      debugM "main" "ZAP done"))
  void $ timeout 1000000 $ killThread forkId
  infoM "main" "Main thread done"
  where
    pipeReaderThread ctx config =
      case (tickPipePath config, pipeReaderQsEndpoint config) of
        (Just pipe, Just qsep) -> do
          tickChan <- newBoundedChan 10000
          bracket (startPipeReader (T.pack pipe) tickChan) stopPipeReader (\_ -> do
            bracket (startQuoteSourceServer tickChan ctx (T.pack qsep)) stopQuoteSourceServer (\_ -> threadDelay 1000000))
        _ -> return ()
      

loadCertificatesFromDirectory :: FilePath -> IO [CurveCertificate]
loadCertificatesFromDirectory filepath = do
  files <- listDirectory filepath
  catMaybes <$> forM files (\file -> hush <$> loadCertificateFromFile file)

