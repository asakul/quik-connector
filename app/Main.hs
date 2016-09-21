module Main where

import Lib
import QuoteSource.DDE
import QuoteSource.DataImport
import Control.Concurrent hiding (readChan)
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Control.Concurrent.BoundedChan
import Data.ATrade

callback :: DdeCallback
callback = undefined

main :: IO ()
main = do
  chan <- newBoundedChan 1000
  forkIO $ forever $ do
    tick <- readChan chan
    when (datatype tick == Price) $ print tick
  dis <- initDataImportServer chan "atrade"
  void initGUI
  window <- windowNew
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False
  widgetShowAll window
  mainGUI

