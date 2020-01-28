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
import           Data.GI.Base
import qualified GI.Gtk                                        as Gtk
import           QuoteSource.DataImport
import           QuoteSource.PipeReader
import           QuoteSource.TableParser
import           QuoteSource.TableParsers.AllParamsTableParser

import           ATrade.Broker.Server
import           ATrade.Broker.TradeSinks.ZMQTradeSink
import           Broker.PaperBroker
import           Broker.QuikBroker

import           System.Directory
import           System.Log.Formatter
import           System.Log.Handler                            (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           System.Timeout
import           System.ZMQ4
import           System.ZMQ4.ZAP

import           Data.Maybe
import qualified Data.Text                                     as T
import           Data.Version

import           ATrade                                        (libatrade_gitrev,
                                                                libatrade_version)
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


initLogging :: IO ()
initLogging = do
  handler <- streamHandler stderr DEBUG >>=
    (\x -> return $
      setFormatter x (simpleLogFormatter "$utcTime\t {$loggername} <$prio> -> $msg"))
  fhandler <- fileHandler "quik-connector.log" DEBUG >>=
    (\x -> return $
      setFormatter x (simpleLogFormatter "$utcTime\t {$loggername} <$prio> -> $msg"))

  hSetBuffering stderr LineBuffering
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  updateGlobalLogger rootLoggerName (setHandlers [handler, fhandler])

main :: IO ()
main = do
  initLogging
  infoM "main" $ "Starting quik-connector-" ++ T.unpack quikConnectorVersionText ++ "; libatrade-" ++ showVersion libatrade_version ++ "(" ++ libatrade_gitrev ++ ")"
  infoM "main" "Loading config"
  config <- readConfig "quik-connector.config.json"

  infoM "main" "Config loaded"
  chan <- newBoundedChan 10000
  infoM "main" "Starting data import server"
  _ <- initDataImportServer [MkTableParser $ mkAllParamsTableParser "allparams"] chan "atrade"

  (forkId, c0, c1, c2) <- forkBoundedChan 10000 chan

  withContext (\ctx -> do
    tickTable <- mkTickTable c0 ctx (T.pack $ qtisEndpoint config)
    brokerQ <- mkQuikBroker tickTable (dllPath config) (quikPath config) (quikAccounts config) (commissions config)
    brokerP <- mkPaperBroker tickTable c1 1000000 ["demo"] (commissions config)
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
              warningM "main" $ "Unable to load server certificate: " ++ errorMessage
              return Nothing
            Right cert -> return $ Just cert
        Nothing -> return Nothing
      let serverParams = defaultServerSecurityParams { sspDomain = Just "global",
        sspCertificate = serverCert }

      bracket (forkIO $ pipeReaderThread ctx config c2) killThread (\_ -> do
        withZMQTradeSink ctx (tradeSink config) (\zmqTradeSink -> do
          withZMQTradeSink ctx (tradeSink2 config) (\zmqTradeSink2 -> do
            bracket (startQuoteSourceServer c2 ctx (T.pack $ quotesourceEndpoint config) quoteSourceServerSecurityParams) stopQuoteSourceServer (\_ -> do
              bracket (startBrokerServer [brokerP, brokerQ] ctx (T.pack $ brokerserverEndpoint config) [zmqTradeSink2, zmqTradeSink] serverParams) stopBrokerServer (\_ -> do
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
    pipeReaderThread ctx config qsdataChan =
      case pipeReaderQsEndpoint config of
        Just qsep -> do
          infoM "main" $ "QS: " ++ qsep
          bracket (startPipeReader ctx (T.pack qsep) qsdataChan) stopPipeReader (\_ -> forever $ threadDelay 1000000)
        _ -> return ()
    quoteSourceServerSecurityParams = defaultServerSecurityParams { sspDomain = Just "global" }


loadCertificatesFromDirectory :: FilePath -> IO [CurveCertificate]
loadCertificatesFromDirectory filepath = do
  files <- listDirectory filepath
  catMaybes <$> forM files (\file -> hush <$> loadCertificateFromFile file)

