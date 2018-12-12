{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import System.Directory (listDirectory)
import Paths_moby
import Data.Coerce
import Data.HashMap.Lazy (HashMap)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as HC
import qualified Data.Text as T

import qualified Configure as Moby

-------------------------------------------------------------------------------

-- lookupMoby :: FilePath -> ByteString -> IO (Maybe ByteString)
-- lookupMoby fp w = (`lookupWord` w) <$> loadMobyTxt fp

lookupWord :: ByteString -> ByteString -> Maybe ByteString
lookupWord ws v = runST $ (`H.lookup` v) =<< mkMobyTable ws

-------------------------------------------------------------------------------

mkMobyTable :: ByteString -> ST s (HC.HashTable s ByteString ByteString)
mkMobyTable = H.fromList . mobyKVs

mkMobyMap :: ByteString -> HashMap ByteString ByteString
mkMobyMap = HM.fromList . mobyKVs

mkMobyMap' :: ByteString -> HashMap Text Text
mkMobyMap' = HM.fromList . fmap (T.pack . C.unpack *** T.pack . C.unpack) . mobyKVs

mobyKVs :: ByteString -> [(ByteString, ByteString)]
mobyKVs = fmap (second (BL.drop 1) . C.span (/= ',')) . C.lines

-------------------------------------------------------------------------------

loadMobyTxt :: FilePath -> IO ByteString
loadMobyTxt = fmap (C.filter (/= '\r') . GZip.decompress) . BL.readFile


