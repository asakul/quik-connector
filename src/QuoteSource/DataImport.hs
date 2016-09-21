{-# LANGUAGE ExistentialQuantification #-}

module QuoteSource.DataImport
(
  initDataImportServer,
  ServerState
) where

import Foreign.Marshal.Alloc
import System.Win32.DLL
import System.Win32.Types
import Foreign
import Foreign.C.Types
import Foreign.C.String

import QuoteSource.DDE
import QuoteSource.XlParser
import QuoteSource.TableParser
import QuoteSource.TableParsers.AllParamsTableParser
import Data.IORef
import Text.Printf
import Data.Binary.Get
import Data.Time.Clock
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as BL

data TableParserInstance = forall a . TableParser a => MkTableParser a
instance TableParser TableParserInstance where
  parseXlTable = parseXlTable
  getTableId (MkTableParser a) = getTableId a
  giveTimestampHint (MkTableParser a) t = MkTableParser (giveTimestampHint a t)


data ServerState = ServerState {
  dde :: DdeState,
  appName :: String,
  parser :: TableParserInstance
}

ddeCallback :: IORef ServerState -> CUInt -> CUInt -> HANDLE -> HANDLE -> HANDLE -> HANDLE -> LPDWORD -> LPDWORD -> IO HANDLE
ddeCallback state msgType format hConv hsz1 hsz2 hData dwData1 dwData2
    | msgType == ddeXtypConnect = handleConnect state hsz1 hsz2
    | msgType == ddeXtypPoke = handlePoke state hsz1 hData
    | otherwise = return nullHANDLE
  where
    handleConnect state hsz1 hsz2 = do
      myAppName <- appName <$> readIORef state
      myDdeState <- dde <$> readIORef state
      maybeAppName <- queryString myDdeState 256 hsz2
      case maybeAppName of
        Just incomingAppName -> do
          putStrLn incomingAppName
          return $ if incomingAppName == myAppName
            then ddeResultTrue
            else ddeResultFalse
        Nothing -> return ddeResultFalse

    handlePoke state hsz1 hData = do
      myDdeState <- dde <$> readIORef state
      maybeTopic <- queryString myDdeState 256 hsz1
      case maybeTopic of
        Nothing -> return ddeResultFalse
        Just topic -> withDdeData hData (\xlData -> case runGetOrFail xlParser $ BL.fromStrict xlData of
          Left (_,  _, errmsg) -> return ddeResultFalse
          Right (_, _, table) -> do
            myParser <- parser <$> readIORef state
            when (topic == getTableId myParser) $ do
              timeHint <- getCurrentTime
              modifyIORef state (\s -> s { parser = giveTimestampHint (parser s) timeHint })
              (MkTableParser myParser) <- parser <$> readIORef state
              let (ticks, newState) = runState (parseXlTable table) myParser
              modifyIORef state (\s -> s { parser = MkTableParser newState })
            return ddeResultAck)


initDataImportServer :: String -> IO (IORef ServerState)
initDataImportServer applicationName = do
  s <- newIORef ServerState { appName = applicationName, dde = nullDdeState, parser = MkTableParser $ mkAllParamsTableParser "allparams" }
  d <- initializeDde applicationName "default" (ddeCallback s)
  modifyIORef s (\state -> state {dde = d})
  putStrLn "DataImportServer initialized"
  return s
