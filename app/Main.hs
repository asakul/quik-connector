{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO

import           ATrade.QuoteSource.Server
import           ATrade.Types
import           Control.Concurrent                            hiding (readChan,
                                                                writeChan)
import           Control.Concurrent.BoundedChan
import           Control.Error.Util
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class                        (MonadIO)
import           Data.GI.Base
import qualified GI.Gtk                                        as Gtk
import           Prelude                                       hiding (log)
import           QuoteSource.DataImport
import           QuoteSource.PipeReader
import           QuoteSource.TableParser
import           QuoteSource.TableParsers.AllParamsTableParser

import           ATrade.Broker.Server
import           ATrade.Broker.TradeSinks.ZMQTradeSink
import           Broker.PaperBroker
import           Broker.QuikBroker

import           System.Directory
import           System.Timeout
import           System.ZMQ4
import           System.ZMQ4.ZAP

import           Data.Maybe
import qualified Data.Text                                     as T
import           Data.Version

import           ATrade                                        (libatrade_gitrev,
                                                                libatrade_version)
import           ATrade.Logging                                (Message, Severity (Debug, Info, Warning),
                                                                fmtMessage,
                                                                logWith)
import           Colog                                         (LogAction,
                                                                logTextStdout,
                                                                (>$<))
import           Colog.Actions                                 (logTextHandle)
import           Config
import           TickTable                                     (mkTickTable)
import           Version

forkBoundedChan :: Int -> BoundedChan Tick -> IO (ThreadId, BoundedChan Tick, BoundedChan Tick, BoundedChan QuoteSourceServerData)
forkBoundedChan size sourceChan = do
  sink1 <- newBoundedChan size
  sink2 <- newBoundedChan size
  sinkQss <- newBoundedChan size
  tid <- forkIO $ forever $ do
      v <- readChan sourceChan
      writeChan sink1 v
      writeChan sink2 v
      writeChan sinkQss (QSSTick v)

  return (tid, sink1, sink2, sinkQss)

mkLogger :: (MonadIO m) => Handle -> LogAction m Message
mkLogger h = fmtMessage >$< (logTextStdout <> logTextHandle h)

main :: IO ()
main = do
  withFile "quik-connector.log" AppendMode $ \logH -> do
    let logger = mkLogger logH
    let log = (logWith logger)
    log Info "main" $ "Starting quik-connector-" <>
                      quikConnectorVersionText <>
                      "; libatrade-" <>
                      (T.pack . showVersion) libatrade_version <>
                      "(" <>
                      T.pack libatrade_gitrev <>
                      ")"
    log Info "main" "Loading config"
    config <- readConfig "quik-connector.config.json"

    log Info "main" "Config loaded"
    chan <- newBoundedChan 10000
    log Info "main" "Starting data import server"
    _ <- initDataImportServer [MkTableParser $ mkAllParamsTableParser "allparams"] chan "atrade"

    (forkId, c0, c1, c2) <- forkBoundedChan 10000 chan

    withContext (\ctx -> do
      tickTable <- mkTickTable c0 ctx (T.pack $ qtisEndpoint config)
      brokerQ <- mkQuikBroker tickTable (dllPath config) (quikPath config) (quikAccounts config) (commissions config) logger
      brokerP <- mkPaperBroker tickTable c1 1000000 ["demo"] (commissions config) logger
      withZapHandler ctx (\zap -> do
        zapSetWhitelist zap "global" $ whitelist config
        zapSetBlacklist zap "global" $ blacklist config

        case brokerClientCertificateDir config of
          Just certFile -> do
            certs <- loadCertificatesFromDirectory certFile
            forM_ certs (\cert -> zapAddClientCertificate zap "global" cert)
          Nothing -> return ()

        serverCert <- case brokerServerCertPath config of
          Just certFile -> do
            eitherCert <- loadCertificateFromFile certFile
            case eitherCert of
              Left errorMessage -> do
                log Warning "main" $ "Unable to load server certificate: " <> T.pack errorMessage
                return Nothing
              Right cert -> return $ Just cert
          Nothing -> return Nothing
        let serverParams = defaultServerSecurityParams { sspDomain = Just "global",
          sspCertificate = serverCert }

        bracket (forkIO $ pipeReaderThread ctx config c2 logger) killThread (\_ -> do
          withZMQTradeSink ctx (tradeSink config) (\zmqTradeSink -> do
            withZMQTradeSink ctx (tradeSink2 config) (\zmqTradeSink2 -> do
              bracket (startQuoteSourceServer c2 ctx (T.pack $ quotesourceEndpoint config) quoteSourceServerSecurityParams) stopQuoteSourceServer (\_ -> do
                bracket (startBrokerServer
                           [brokerP, brokerQ]
                           ctx
                           (T.pack $ brokerserverEndpoint config)
                           (T.pack $ brokerNotificationsEndpoint config)
                           [zmqTradeSink2, zmqTradeSink]
                           serverParams
                           logger) stopBrokerServer (\_ -> do
                  void $ Gtk.init Nothing
                  window <- new Gtk.Window [ #title := "Quik connector" ]
                  void $ on window #destroy Gtk.mainQuit
                  #showAll window
                  Gtk.main)
                log Info "main" "BRS down")
              log Debug "main" "QS done")
            log Debug "main" "TGTS done")
          log Debug "main" "ZMQTS done")
        log Debug "main" "ZAP done"))
    void $ timeout 1000000 $ killThread forkId
    log Info "main" "Main thread done"
  where
    pipeReaderThread ctx config qsdataChan logger =
      case pipeReaderQsEndpoint config of
        Just qsep -> do
          logWith logger Info "main" $ "QS: " <> T.pack qsep
          bracket (startPipeReader ctx (T.pack qsep) qsdataChan logger) stopPipeReader (\_ -> forever $ threadDelay 1000000)
        _ -> return ()
    quoteSourceServerSecurityParams = defaultServerSecurityParams { sspDomain = Just "global" }


loadCertificatesFromDirectory :: FilePath -> IO [CurveCertificate]
loadCertificatesFromDirectory filepath = do
  files <- listDirectory filepath
  catMaybes <$> forM files (\file -> hush <$> loadCertificateFromFile file)

