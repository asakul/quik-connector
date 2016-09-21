
module Data.ATrade (
  Tick(..),
  DataType(..)
) where

import Data.Decimal
import Data.Time.Clock

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

