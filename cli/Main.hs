{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Options.Applicative
import Data.Text (Text)
import Data.Maybe
import Data.Either
import Control.Monad (void)
import Text.Printf
import System.Directory
import System.FilePath
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Download as D
import qualified Version as V

import qualified Client

eitherAsMaybe :: Either a b -> Maybe b
eitherAsMaybe = \case
  Left _ -> Nothing
  Right a -> Just a

main :: IO ()
main = execParser opts >>= \mos ->
  case (version mos, word mos, useFS mos) of
    (True,  _,    _) -> print V.version
    (   _, ws, True) -> do
      cacheDir <- getXdgDirectory XdgCache "moby"
      createDirectoryIfMissing True cacheDir
      D.fetchFiles cacheDir
      bs <- Lib.loadMobyTxt (cacheDir </> "words.txt.gz")
      void $ go mos (T.unwords ws) (pure . lookupWord bs)

    (   _, ws,    _) ->
      go mos (T.unwords ws) (fmap eitherAsMaybe . Client.lookup)
      >>= \case
        Just () -> pure ()
        Nothing -> Client.healthCheck >>= \case
          Left _ -> putStrLn "server is down. Start server with `mobyd`"
          Right _ -> pure ()

  where
    opts = info (mobyOpts <**> helper)
       ( fullDesc
       <> progDesc "look up synonyms for WORD"
       <> header "moby -- look up words from the Moby Project"
       )

    go :: MobyOpts -> Text -> (Text -> IO (Maybe Text)) -> IO (Maybe ())
    go mos w _lookup = _lookup w >>= \case
      Nothing -> printf "%s not found in moby!\n" (show w) >> pure Nothing
      Just ss -> do
        let hdr = show w ++ " synonyms:"
        putStrLn   hdr
        putStrLn $ replicate (length hdr) '='
        putStrLn . T.unpack $ T.intercalate (sep mos) (T.split (== ',') ss)
        pure $ Just ()


data MobyOpts = MobyOpts
  { sep :: Text
  , version :: Bool
  , useFS :: Bool
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
  <*> switch
      ( long "use-fs"
      <> short 'f'
      )
  <*> some (argument str (metavar "WORD"))

