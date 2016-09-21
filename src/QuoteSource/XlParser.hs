
module QuoteSource.XlParser (
  XlData(..),
  xlParser
) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.ByteString hiding (concat, unpack)
import Data.List as L
import Data.Word
import Data.Text as T hiding (concat)
import Data.Text.Encoding

data XlData = XlInt Int | XlDouble Double | XlString String | XlEmpty
  deriving (Eq, Show)

data XlPosition = XlPosition { width :: Int, height :: Int, xPos :: Int, yPos :: Int }

btTable = 16
btFloat = 1
btString = 2
btBlank = 5
btInt = 6
btSkip = 7

xlParser :: Get (Int, Int, [XlData])
xlParser = do
  datatype <- getWord16le
  when (datatype /= btTable) $ fail "First entry should be table"
  blocksize <- fromEnum <$> getWord16le
  when (blocksize /= 4) $ fail "Table entry should have size 4"
  height <- getWord16le
  width <- getWord16le
  table <- parseTable
  return (fromEnum width, fromEnum height, table)
  where
    parseTable :: Get [XlData]
    parseTable = concat <$> parseTable'

    parseTable' :: Get [[XlData]]
    parseTable' = do
      eof <- isEmpty
      if eof
        then return []
        else do
          cells <- parseEntry
          rest <- parseTable'
          return $ cells : rest

    parseEntry :: Get [XlData]
    parseEntry = do
      datatype <- getWord16le
      blocksize <- fromEnum <$> getWord16le
      parseEntry' datatype blocksize

    parseEntry' :: Word16 -> Int -> Get [XlData]
    parseEntry' datatype blocksize
      | datatype == btFloat = parseFloats blocksize
      | datatype == btString = parseStrings blocksize
      | datatype == btBlank = parseBlanks blocksize
      | otherwise = fail $ "Unknown field type: " ++ show datatype

    parseFloats blocksize = do
      float <- getFloat64le
      if blocksize - 8 <= 0
        then return [XlDouble float]
        else do
          rest <- parseFloats (blocksize - 8)
          return $ XlDouble float : rest

    parseStrings blocksize = do
      length <- fromEnum <$> getWord8
      s <- unpack . decodeUtf8 <$> getByteString length
      if length + 1 >= blocksize
        then return [XlString s]
        else do
          rest <- parseStrings (blocksize - length - 1)
          return $ XlString s : rest

    parseBlanks blocksize = do
      fields <- fromEnum <$> getWord16le
      return $ L.replicate fields XlEmpty

