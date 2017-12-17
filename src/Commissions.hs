{-# LANGUAGE OverloadedStrings #-}

module Commissions (
  CommissionConfig(..)
) where

import Data.Aeson
import qualified Data.Text as T

data CommissionConfig = CommissionConfig {
  comPrefix :: T.Text,
  comPercentage :: Double,
  comFixed :: Double
} deriving (Show)

instance FromJSON CommissionConfig where
  parseJSON = withObject "object" (\obj -> CommissionConfig <$> 
    obj .: "prefix" <*>
    obj .:? "percentage" .!= 0 <*>
    obj .:? "fixed" .!= 0 )

