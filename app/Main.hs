module Main where

import Lib
import QuoteSource.DDE
import QuoteSource.DataImport
import Control.Concurrent
import Control.Monad

import Graphics.UI.FLTK.LowLevel.FL
import Graphics.UI.FLTK.LowLevel.FLTKHS

callback :: DdeCallback
callback = undefined

main :: IO ()
main = do
  dis <- initDataImportServer "atrade"
  forever $ threadDelay 1000
  window <- windowNew (Size (Width 320) (Height 170))
                Nothing Nothing
  end window
  showWidget window
  _ <- run
  return ()

