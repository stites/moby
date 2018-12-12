{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Options.Applicative
import Data.ByteString.Lazy (ByteString)
import Text.Printf
import System.Directory
import System.FilePath
import System.IO
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap.Lazy as HM

import qualified Lib
import qualified Version as V
import qualified Download as D
import qualified Server


main :: IO ()
main = execParser opts >>= \mos ->
  if version mos
  then print V.version
  else do
    cacheDir <- getXdgDirectory XdgCache "moby"
    createDirectoryIfMissing True cacheDir
    D.fetchFiles cacheDir

    run =<< Server.Env . Lib.mkMobyMap' <$> Lib.loadMobyTxt (cacheDir </> "words.txt.gz")

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
  opts
    = info (mobyOpts <**> helper)
    ( fullDesc
    <> progDesc "Start server to query moby thesaurus"
    <> header "mobyd -- backend to the moby cli"
    )

  run :: Server.Env -> IO ()
  run env = do
    runSettings settings =<< pure (Server.mobyApp env)

  port :: Int
  port = 3000
  settings =
    setPort port $
    setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
    defaultSettings

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

