
module Data.ATrade (
) where

data DataType = Price | OpenInterest | BestBid | BestOffer | Depth | TheoryPrice | Volatility | TotalSupply | TotalDemand

data Tick = Tick {
  datatype :: DataType,

}
