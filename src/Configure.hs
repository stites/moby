module Configure where

import Data.Maybe
import Dhall
import Network.URI

data MobyConf = MobyConf
  { callexternal :: [ URLHook ]
  , dbLocation :: String
  }

data URLHook = URLHook
  { location :: URI
  , run :: MobyConf -> IO ()
  }

defaultConfig :: MobyConf
defaultConfig = MobyConf [] "~/.config/moby/"

config :: IO MobyConf
config = fromMaybe defaultConfig <$> findConfig

findConfig :: IO (Maybe MobyConf)
findConfig = pure Nothing
