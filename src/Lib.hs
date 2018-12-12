{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import System.Directory (listDirectory)
import Paths_moby
import Data.Coerce
import Data.HashMap.Lazy (HashMap)
import Data.ByteString.Lazy (ByteString)
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as HC

import qualified Configure as Moby

lookupMoby :: ByteString -> IO (Maybe ByteString)
lookupMoby w = (`lookupWord` w) <$> loadMobyTxt

lookupWord :: ByteString -> ByteString -> Maybe ByteString
lookupWord ws v = runST $ (`H.lookup` v) =<< mkMobyTable ws

getWordsTxt :: IO FilePath
getWordsTxt = (++ "words.txt.gz") <$> Paths_moby.getDataDir

loadMobyTxt :: IO ByteString
loadMobyTxt = getWordsTxt >>= fmap (C.filter (/= '\r') . GZip.decompress) . BL.readFile

mobyKVs :: ByteString -> [(ByteString, ByteString)]
mobyKVs = fmap (second (BL.drop 1) . C.span (/= ',')) . C.lines

mkMobyTable :: ByteString -> ST s (HC.HashTable s ByteString ByteString)
mkMobyTable = H.fromList . mobyKVs

mkMobyMap :: ByteString -> HashMap ByteString ByteString
mkMobyMap = HM.fromList . mobyKVs

