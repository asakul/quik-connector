{-# LANGUAGE OverloadedStrings #-}

module ATrade.Quotes.QTIS
(
  TickerInfo(..),
  qtisGetTickersInfo,
  qtisGetTickersInfo'
) where

import           ATrade.Types
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy  as BL
import           Data.Maybe
import qualified Data.Text             as T
import           System.ZMQ4

data TickerInfo = TickerInfo {
  tiTicker   :: T.Text,
  tiLotSize  :: Integer,
  tiTickSize :: Price
} deriving (Show, Eq)

instance FromJSON TickerInfo where
  parseJSON = withObject "object" (\obj ->
    TickerInfo <$>
      obj .: "ticker" <*>
      obj .: "lot_size" <*>
      obj .: "tick_size")

instance ToJSON TickerInfo where
  toJSON ti = object [ "ticker" .= tiTicker ti,
    "lot_size" .= tiLotSize ti,
    "tick_size" .= tiTickSize ti ]

qtisGetTickersInfo' :: T.Text -> [TickerId] -> IO [TickerInfo]
qtisGetTickersInfo' endpoint tickers = withContext (\ctx -> qtisGetTickersInfo ctx endpoint tickers)

qtisGetTickersInfo :: Context -> T.Text -> [TickerId] -> IO [TickerInfo]
qtisGetTickersInfo ctx endpoint tickers =
  withSocket ctx Req (\sock -> do
    connect sock $ T.unpack endpoint
    catMaybes <$> forM tickers (\tickerId -> do
      send sock [] $ BL.toStrict (tickerRequest tickerId)
      response <- receiveMulti sock
      let r = parseResponse response
      return r))
  where
    tickerRequest tickerId = encode $ object ["ticker" .= tickerId]
    parseResponse :: [BC8.ByteString] -> Maybe TickerInfo
    parseResponse (header:payload:_) = if header == "OK"
      then decode $ BL.fromStrict payload
      else Nothing
    parseResponse _ = Nothing

