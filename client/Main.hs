{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Options.Applicative
import Data.ByteString.Lazy (ByteString)
import Text.Printf
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Version as V

main :: IO ()
main = execParser opts >>= \mos ->
  case (version mos, word mos) of
    (True,  _) -> print V.version
    (   _, ws) ->
      let w = C.unwords ws
      in (`lookupWord` w) <$> loadMobyTxt
        >>= \case
          Nothing -> printf "%s not found in moby!\n" (show w)
          Just s  -> do
            let hdr = show w ++ " synonyms:"
            putStrLn   hdr
            putStrLn $ replicate (length hdr) '='
            putStrLn . C.unpack $ C.intercalate (sep mos) (C.split ',' s)
 where
   opts = info (mobyOpts <**> helper)
      ( fullDesc
      <> progDesc "look up synonyms for WORD"
      <> header "moby -- look up words from the Moby Project"
      )

data MobyOpts = MobyOpts
  { sep :: ByteString
  , version :: Bool
  , word :: [ByteString]
  }

mobyOpts :: Parser MobyOpts
mobyOpts = MobyOpts
  <$> strOption
      ( long "sep"
      <> short 's'
      <> value "\n"
      <> metavar "SEPERATOR"
      <> help "seperator for words"
      <> showDefault
      )
  <*> switch
      ( long "version"
      <> short 'v'
      <> help "Show the version"
      )
  <*> some (argument str (metavar "WORD"))

