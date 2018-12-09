{-# LANGUAGE QuasiQuotes #-}

module Version
  (
    quikConnectorVersion,
    quikConnectorVersionText
  ) where

import qualified Data.Text      as T
import           Text.Printf.TH

quikConnectorVersion :: (Int, Int, Int, Int)
quikConnectorVersion = (0, 2, 3, 0)

quikConnectorVersionText :: T.Text
quikConnectorVersionText =
  [st|%d.%d.%d.%d|] v1 v2 v3 v4
  where
    (v1, v2, v3, v4) = quikConnectorVersion

