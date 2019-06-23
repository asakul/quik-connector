
module System.Win32.XlParser (
  XlData(..),
  xlParser
) where

import           Codec.Text.IConv
import           Control.Applicative
import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.IEEE754
import           Data.ByteString      hiding (concat, unpack)
import qualified Data.ByteString.Lazy as BL
import           Data.List            as L
import           Data.Text            as T hiding (concat)
import           Data.Text.Encoding
import           Data.Word

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
      s <- convert "CP1251" "UTF-8" . BL.fromStrict <$> getByteString length
      case decodeUtf8' (BL.toStrict s) of
        Left err -> fail $ "Can't parse utf8: " ++ show err
        Right bs -> do
          let s = unpack bs
          if length + 1 >= blocksize
            then return [XlString s]
            else do
              rest <- parseStrings (blocksize - length - 1)
              return $ XlString s : rest

    parseBlanks blocksize = do
      fields <- fromEnum <$> getWord16le
      return $ L.replicate fields XlEmpty

