{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Options.Applicative
import Data.Text (Text)
import Text.Printf
import qualified Data.Text as T

import qualified Version as V

import qualified Client

main :: IO ()
main = execParser opts >>= \mos ->
  case (version mos, word mos) of
    (True,  _) -> print V.version
    (   _, ws) ->
      let w = T.unwords ws
      in Client.lookup w
        >>= \case
          Left err -> printf "%s not found in moby!\n" (show w)
          Right ss -> do
            let hdr = show w ++ " synonyms:"
            putStrLn   hdr
            putStrLn $ replicate (length hdr) '='
            putStrLn . T.unpack $ T.intercalate (sep mos) (T.split (== ',') ss)
 where
   opts = info (mobyOpts <**> helper)
      ( fullDesc
      <> progDesc "look up synonyms for WORD"
      <> header "moby -- look up words from the Moby Project"
      )

data MobyOpts = MobyOpts
  { sep :: Text
  , version :: Bool
  , word :: [Text]
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

