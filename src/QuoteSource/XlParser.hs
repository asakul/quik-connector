
module QuoteSource.XlParser (
  XlData(..),
  xlParser
) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.ByteString

data XlData = XlInt Int | XlDouble Double | XlString String | XlEmpty

data XlPosition = XlPosition { width :: Int, height :: Int, xPos :: Int, yPos :: Int }

btTable = 16
btFloat = 1
btString = 2
btBlank = 5
btInt = 6
btSkip = 7

incrementPosition :: XlPosition -> Maybe XlPosition
incrementPosition pos = do
  if 1 + xPos pos < width pos
    then Just pos { xPos = 1 + xPos pos }
    else if 1 + yPos pos < height pos
      then Just pos { xPos = 0, yPos = 1 + yPos pos }
      else Nothing

xlParser :: Get (Int, Int, [XlData])
xlParser = do
  datatype <- getWord16le
  when (datatype /= btTable) $ fail "First entry should be table"
  blocksize <- fromEnum <$> getWord16le
  when (blocksize /= 4) $ fail "Table entry should have size 4"
  return (0, 0, [])


