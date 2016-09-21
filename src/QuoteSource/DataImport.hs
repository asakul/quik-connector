{-# LANGUAGE ExistentialQuantification #-}

module QuoteSource.DataImport
(
  ServerState,
  initDataImportServer,
  shutdownDataImportServer
) where

import Control.Concurrent.BoundedChan
import Control.Monad.State.Strict
import Data.ATrade
import Data.IORef
import Data.Time.Clock
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import QuoteSource.DDE
import QuoteSource.TableParser
import QuoteSource.TableParsers.AllParamsTableParser
import QuoteSource.XlParser
import System.Win32.Types
import Text.Printf

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M

data TableParserInstance = forall a . TableParser a => MkTableParser a

data ServerState = ServerState {
  appName :: String,
  parsers :: IORef (M.Map String TableParserInstance),
  tickChannel :: BoundedChan Tick
}

ddeCallback :: ServerState -> String -> (Int, Int, [XlData]) -> IO Bool
ddeCallback state topic table = do
  myParsers <- readIORef $ parsers state
  case M.lookup topic myParsers of
    Just (MkTableParser myParser) -> do
      timeHint <- getCurrentTime
      let stateWithTimeHint = giveTimestampHint myParser timeHint
      let (ticks, newState) = runState (parseXlTable table) $ stateWithTimeHint
      modifyIORef (parsers state) (\m -> M.insert topic (MkTableParser newState) m)
      writeList2Chan (tickChannel state) ticks
      return True
    _ -> return False


initDataImportServer :: BoundedChan Tick -> String -> IO (ServerState, IORef DdeState)
initDataImportServer tickChan applicationName = do
  parsers <- newIORef $ M.fromList $ map (\p -> (getTableId p, MkTableParser p)) [mkAllParamsTableParser "allparams"]
  let s = ServerState { appName = applicationName, parsers = parsers, tickChannel = tickChan }
  d <- initializeDde applicationName "default" (ddeCallback s)
  return (s, d)

shutdownDataImportServer :: (ServerState, IORef DdeState) -> IO ()
shutdownDataImportServer (state, dde) = readIORef dde >>= destroyDde

