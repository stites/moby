{-# LANGUAGE OverloadedStrings #-}
module Lib where

import System.Directory (listDirectory)
import Paths_moby
import Data.Coerce
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Configure as Moby

someFunc :: IO ()
someFunc = do
  ls <- loadMobyLines
  print $ take 2 ls

loadMobyTxt :: IO BL.ByteString
loadMobyTxt = C.filter (/= '\r') . GZip.decompress <$> BL.readFile "data/words.txt.gz"

loadMobyLines :: IO [BL.ByteString]
loadMobyLines = C.lines <$> loadMobyTxt

