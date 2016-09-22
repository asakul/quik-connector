
module Data.ATrade (
  Tick(..),
  DataType(..),
  serializeTick
) where

import Data.Decimal
import Data.Time.Clock
import Data.DateTime
import Data.ByteString.Lazy as B
import Data.Text as T
import Data.Text.Encoding as E
import Data.List as L
import Data.Binary.Builder

data DataType = Unknown
  | Price
  | OpenInterest
  | BestBid
  | BestOffer
  | Depth
  | TheoryPrice
  | Volatility
  | TotalSupply
  | TotalDemand
  deriving (Show, Eq)

instance Enum DataType where
  fromEnum x
    | x == Price = 1
    | x == OpenInterest = 3
    | x == BestBid = 4
    | x == BestOffer = 5
    | x == Depth = 6
    | x == TheoryPrice = 7
    | x == Volatility = 8
    | x == TotalSupply = 9
    | x == TotalDemand = 10
    | x == Unknown = -1
    | otherwise = -1

  toEnum x
    | x == 1 = Price
    | x == 3 = OpenInterest
    | x == 4 = BestBid
    | x == 5 = BestOffer
    | x == 6 = Depth
    | x == 7 = TheoryPrice
    | x == 8 = Volatility
    | x == 9 = TotalSupply
    | x == 10 = TotalDemand
    | otherwise = Unknown

data Tick = Tick {
  security :: String,
  datatype :: DataType,
  timestamp :: UTCTime,
  value :: Decimal,
  volume :: Integer
} deriving (Show, Eq)

serializeTick :: Tick -> [ByteString]
serializeTick tick = header : [rawdata]
  where
    header = B.fromChunks [ E.encodeUtf8 . T.pack $ security tick ]
    rawdata = toLazyByteString $ mconcat [ 
      putWord32le 1,
      putWord64le $ fromIntegral . toSeconds . timestamp $ tick,
      putWord32le $ fromIntegral . truncate . (* 1000000) . fractionalPart . utctDayTime . timestamp $ tick,
      putWord32le $ fromIntegral . fromEnum . datatype $ tick,
      putWord64le $ truncate . value $ tick,
      putWord32le $ truncate . (* 1000000000) . fractionalPart $ value tick,
      putWord32le $ fromIntegral $ volume tick ]
    fractionalPart :: (RealFrac a) => a -> a
    fractionalPart x = x - fromIntegral (floor x)
  
