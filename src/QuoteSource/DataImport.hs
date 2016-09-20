
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
import Data.IORef
import Text.Printf
import Data.Binary

data ServerState = ServerState {
  dde :: DdeState,
  appName :: String
}

ddeCallback :: IORef ServerState -> CUInt -> CUInt -> HANDLE -> HANDLE -> HANDLE -> HANDLE -> LPDWORD -> LPDWORD -> IO HANDLE
ddeCallback state msgType format hConv hsz1 hsz2 hData dwData1 dwData2
    | msgType == ddeXtypConnect = handleConnect state hsz1 hsz2 
    | msgType == ddeXtypPoke = handlePoke state hsz1 hData
    | otherwise = do
      putStrLn $ printf "msgtype: %08x" $ toInteger msgType
      return nullHANDLE
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

initDataImportServer :: String -> IO (IORef ServerState)
initDataImportServer applicationName = do
  s <- newIORef ServerState { appName = applicationName, dde = nullDdeState }
  d <- initializeDde applicationName "default" (ddeCallback s)
  modifyIORef s (\state -> state {dde = d})
  putStrLn "DataImportServer initialized"
  return s
