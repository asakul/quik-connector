{-# LANGUAGE ExistentialQuantification #-}

module QuoteSource.TableParser (
  TableParser(..),
  TableParserInstance(..)
) where

import QuoteSource.XlParser
import Data.ATrade
import Control.Monad.State.Strict
import Data.Time.Clock

data TableParserInstance = forall a . TableParser a => MkTableParser a

class TableParser a where
  parseXlTable :: (Int, Int, [XlData]) -> State a [Tick]
  giveTimestampHint :: a -> UTCTime -> a
  getTableId :: a -> String
  
