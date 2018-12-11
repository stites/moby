module Version where

import Data.List (intercalate)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Version as V
import qualified Paths_moby

newtype Version = Version  { getPathsVersion :: V.Version }

instance Show Version where
  show (Version (V.Version v t)) = release
    where
      release = "moby-" ++ intercalate "." (show <$> v)

versionBranch :: Version -> [Int]
versionBranch = V.versionBranch . unsafeCoerce

