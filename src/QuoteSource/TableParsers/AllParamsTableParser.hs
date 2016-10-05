{-# LANGUAGE OverloadedStrings #-}

module QuoteSource.TableParsers.AllParamsTableParser (
  AllParamsTableParser,
  mkAllParamsTableParser
) where

import qualified Data.Map.Strict as M
import QuoteSource.TableParser
import ATrade.Types
import System.Win32.XlParser
import Data.Tuple
import Data.Decimal
import Control.Monad.State.Strict
import Control.DeepSeq
import Data.Time.Clock
import Data.Maybe
import Data.DateTime
import qualified Data.Text as T

data TableColumn = CUnknown
  | CTicker
  | CClassCode
  | CPrice
  | CBestBid
  | CBestAsk
  | CTotalSupply
  | CTotalDemand
  | COpenInterest
  | CVolume
  deriving (Eq, Show, Ord)

columnCodes = M.fromList [
  ("CLASS_CODE", CClassCode),
  ("CODE", CTicker),
  ("BID", CBestBid),
  ("OFFER", CBestAsk),
  ("LAST", CPrice),
  ("NUMCONTRACTS", COpenInterest),
  ("BIDDEPTHT", CTotalDemand),
  ("OFFERDEPTHT", CTotalSupply),
  ("VOLTODAY", CVolume)]

columnToDataType :: TableColumn -> DataType
columnToDataType x 
  | x == CPrice = Price
  | x == CBestBid = BestBid
  | x == CBestAsk = BestOffer
  | x == CTotalSupply = TotalSupply
  | x == CTotalDemand = TotalDemand
  | x == COpenInterest = OpenInterest
  | otherwise = Unknown


type TableSchema = M.Map TableColumn Int

data AllParamsTableParser = AllParamsTableParser {
  schema :: Maybe TableSchema,
  tableId :: String,
  volumes :: M.Map T.Text Integer,
  timestampHint :: UTCTime
}

mkAllParamsTableParser id = AllParamsTableParser {
  schema = Nothing,
  tableId = id,
  volumes = M.empty,
  timestampHint = startOfTime }

securityName :: String -> String -> T.Text
securityName classCode ticker = T.pack $ classCode ++ ('#' : ticker)

parseSchema (width, height, cells) = M.fromList . zipWith (curry swap) [0..] $ map parseSchemaItem . take width $ cells
  where
    parseSchemaItem cell = case cell of
      XlString s -> M.findWithDefault CUnknown s columnCodes
      _ -> CUnknown

safeAt :: [a] -> Int -> Maybe a
safeAt list index = if index < 0 || index >= length list
  then Nothing
  else Just $ list !! index

parseWithSchema :: TableSchema -> (Int, Int, [XlData]) -> State AllParamsTableParser [Tick]
parseWithSchema sch (width, height, cells) = do
  ticks <- mapM parseRow $ groupByN width cells
  return . concat $ ticks
  where
    parseRow :: [XlData] -> State AllParamsTableParser [Tick]
    parseRow row = case (getClassCode row, getTicker row) of
      (Just classCode, Just ticker) -> do
        maybeticks <- mapM (\f -> f row classCode ticker) parsers
        return $ catMaybes maybeticks
      _ -> return []

    parsers :: [[XlData] -> String -> String -> State AllParamsTableParser (Maybe Tick)]
    parsers = parsePrice : map parseValue [CBestBid, CBestAsk, COpenInterest, CTotalDemand, CTotalSupply]

    parseValue :: TableColumn -> [XlData] -> String -> String -> State AllParamsTableParser (Maybe Tick)
    parseValue columnType row classCode ticker = case M.lookup columnType sch of
      Nothing -> return Nothing
      Just index -> case row `safeAt` index of
          Just (XlDouble value) -> do
            ts <- gets timestampHint
            return $ Just Tick {
              security = force $ securityName classCode ticker,
              datatype = columnToDataType columnType,
              timestamp = ts,
              value = force $ realFracToDecimal 10 value,
              volume = 0 }
          _ -> return Nothing

    parsePrice :: [XlData] -> String -> String -> State AllParamsTableParser (Maybe Tick)
    parsePrice row classCode ticker = case M.lookup CPrice sch of
      Nothing -> return Nothing
      Just index -> case row `safeAt` index of
          Just (XlDouble value) -> do
            tickVolume <- calculateTickVolume row $ securityName classCode ticker
            if tickVolume > 0
              then do
                ts <- gets timestampHint
                return $ Just Tick {
                  security = force $ securityName classCode ticker,
                  datatype = Price,
                  timestamp = ts,
                  value = force $ realFracToDecimal 10 value,
                  volume = tickVolume}
              else
                return Nothing
          _ -> return Nothing

    calculateTickVolume :: [XlData] -> T.Text -> State AllParamsTableParser Integer
    calculateTickVolume row secname = case M.lookup CVolume sch of
      Nothing -> return 1
      Just index -> case row `safeAt` index of
        Just (XlDouble volume) -> do
          oldVolumes <- gets volumes
          let intVolume = round volume
          case M.lookup secname oldVolumes of
            Nothing -> do
              modify (\s -> s { volumes = oldVolumes `seq` M.insert secname intVolume oldVolumes } )
              return 1
            Just oldVolume -> do
              modify (\s -> s { volumes = oldVolumes `seq` M.insert secname intVolume oldVolumes } )
              return $ if intVolume > oldVolume
                then intVolume - oldVolume
                else if intVolume < oldVolume
                  then 1
                  else 0
        _ -> return 0

    groupByN :: Int -> [a] -> [[a]]
    groupByN n l = case l of
      [] -> []
      _ -> take n l : groupByN n (drop n l)

    getStringField :: TableColumn -> [XlData] -> Maybe String
    getStringField columnType row = case M.lookup columnType sch of
      Nothing -> Nothing
      Just index -> case row `safeAt` index of
        Just (XlString s) -> Just s
        _ -> Nothing

    getClassCode :: [XlData] -> Maybe String
    getClassCode = getStringField CClassCode

    getTicker :: [XlData] -> Maybe String
    getTicker = getStringField CTicker

instance TableParser AllParamsTableParser where
  parseXlTable table = do
    mySchema <- gets schema
    case mySchema of
      Just sch -> parseWithSchema sch table
      Nothing -> do
        modify (\s -> s { schema = Just $ parseSchema table })
        parseWithSchema (parseSchema table) table

  getTableId = tableId
  giveTimestampHint tp hint = tp { timestampHint = hint }

