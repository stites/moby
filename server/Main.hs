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
  if version mos
  then print V.version
  else do
    undefined
    -- add a pid file (if a pid and lock exist, error)
    -- check to see if there is a lock file (if so warn and overwrite lock file)
    -- add a lock file
    -- look up words.txt file
    -- start socket
    -- wait for queued messages
    -- return results on queued messages
    -- on exit:
    --   - remove lock
    --   - remove pid
    --   - exit 0

 where
   opts = info (mobyOpts <**> helper)
      ( fullDesc
      <> progDesc "Start server to query moby thesaurus"
      <> header "mobyd -- backend to the moby cli"
      )

newtype MobyOpts = MobyOpts
  { version :: Bool
  }

mobyOpts :: Parser MobyOpts
mobyOpts = MobyOpts
  <$> switch
      ( long "version"
      <> short 'v'
      <> help "Show the version"
      )

