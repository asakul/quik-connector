
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
import Data.IORef

data ServerState = ServerState {
  dde :: DdeState,
  appName :: String
}

ddeCallback :: IORef ServerState -> CUInt -> CUInt -> HANDLE -> HANDLE -> HANDLE -> HANDLE -> LPDWORD -> LPDWORD -> IO HANDLE
ddeCallback state msgType format hConv hsz1 hsz2 hData dwData1 dwData2 = do
  putStrLn "Callback"
  return nullHANDLE

{-
    | msgType == ddeXtypConnect = handleConnect state hsz1 hsz2 
    | msgType == ddeXtypPoke = handlePoke state hsz1 hData
    | otherwise = do
      print msgType
      return nullHANDLE
  where
    handleConnect state hsz1 hsz2 = do
      myAppName <- appName <$> readIORef state
      myDdeState <- dde <$> readIORef state
      maybeAppName <- queryString myDdeState 256 hsz2
      putStrLn "Connect"
      case maybeAppName of
        Just incomingAppName -> do
          putStrLn incomingAppName
          return $ if incomingAppName == myAppName
            then ddeResultTrue
            else ddeResultFalse
        Nothing -> return ddeResultFalse

    handlePoke state hsz1 hData = do
      putStrLn "Poke"
      return ddeResultAck
      -}
        

initDataImportServer :: String -> IO (IORef ServerState)
initDataImportServer applicationName = do
  s <- newIORef ServerState { appName = applicationName, dde = nullDdeState }
  d <- initializeDde applicationName "default" (ddeCallback s)
  modifyIORef s (\state -> state {dde = d})
  putStrLn "DataImportServer initialized"
  return s
