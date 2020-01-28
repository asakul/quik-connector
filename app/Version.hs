
module Version
  (
    quikConnectorVersion,
    quikConnectorVersionText
  ) where

import qualified Data.Text            as T
import           Data.Version
import           Paths_quik_connector


quikConnectorVersion :: Version
quikConnectorVersion = version

quikConnectorVersionText :: T.Text
quikConnectorVersionText = T.pack $ showVersion version

