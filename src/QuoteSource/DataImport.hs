
module QuoteSource.DataImport
(
  ServerState,
  initDataImportServer,
  shutdownDataImportServer
) where

import Control.Concurrent.BoundedChan
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.Monad.State.Strict
import ATrade.Types
import Data.IORef
import Data.Time.Clock
import QuoteSource.TableParser
import System.Win32.DDE
import System.Win32.XlParser

import qualified Data.Map as M

data ServerState = ServerState {
  appName :: String,
  parsers :: IORef (M.Map String TableParserInstance),
  tickChannel :: TBQueue Tick
}

ddeCallback :: ServerState -> String -> (Int, Int, [XlData]) -> IO Bool
ddeCallback state topic table = do
  myParsers <- readIORef $ parsers state
  case M.lookup topic myParsers of
    Just (MkTableParser myParser) -> do
      timeHint <- getCurrentTime
      let stateWithTimeHint = giveTimestampHint myParser timeHint
      let (ticks, newState) = runState (parseXlTable table) stateWithTimeHint
      modifyIORef' (parsers state) (\s -> newState `seq` s `seq` M.insert topic (MkTableParser newState) s)
      mapM_ (atomically . writeTBQueue (tickChannel state)) ticks
      return True
    _ -> return False


initDataImportServer :: [TableParserInstance] -> TBQueue Tick -> String -> IO (ServerState, IORef DdeState)
initDataImportServer parsers tickChan applicationName = do
  parsers <- newIORef $ M.fromList $ map (\(MkTableParser p) -> (getTableId p, MkTableParser p)) parsers
  let s = ServerState { appName = applicationName, parsers = parsers, tickChannel = tickChan }
  d <- initializeDde applicationName "default" (ddeCallback s)
  return (s, d)

shutdownDataImportServer :: (ServerState, IORef DdeState) -> IO ()
shutdownDataImportServer (state, dde) = readIORef dde >>= destroyDde

