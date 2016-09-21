
module QuoteSource.TableParser (
  TableParser(..)
) where

import QuoteSource.XlParser
import Data.ATrade
import Control.Monad.State.Strict
import Data.Time.Clock

class TableParser a where
  parseXlTable :: (Int, Int, [XlData]) -> State a [Tick]
  giveTimestampHint :: a -> UTCTime -> a
  getTableId :: a -> String
  
