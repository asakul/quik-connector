module Main where

import Lib
import QuoteSource.DDE
import QuoteSource.DataImport
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)


callback :: DdeCallback
callback = undefined

main :: IO ()
main = do
  dis <- initDataImportServer "atrade"
  void initGUI
  window <- windowNew
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False
  widgetShowAll window
  mainGUI

