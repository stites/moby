{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Exception.Safe (throwString)
import Control.Monad (unless, when, join)
import Control.Monad.Extra (unlessM)
import Data.HashSet (Set)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Distribution.PackageDescription
    ( GenericPackageDescription(..)
    , dataDir
    )
import Distribution.Simple
    ( UserHooks(..)
    , defaultMainWithHooks
    , simpleUserHooks
    )
import System.IO (hPutStrLn, stderr, stdout, hFlush)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Text.Printf (printf)
import Network.HTTP.Client (httpLbs, parseRequest, responseStatus, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.HashSet as HS
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Lazy as B
import qualified Codec.Compression.GZip as GZip


main :: IO ()
main = defaultMainWithHooks $ hooks
  { confHook = \gh@(g, _) c -> fetchFiles g >> confHook simpleUserHooks gh c
  }
 where
  hooks :: UserHooks
  hooks = simpleUserHooks


fetchFiles :: GenericPackageDescription -> IO ()
fetchFiles g = mapM_ (maybeDownload g) fileInfos


getFilePath :: GenericPackageDescription -> FilePath -> FilePath
getFilePath g basename = dataDir (packageDescription g) ++ basename


getUrl :: FilePath -> String
getUrl basename = baseUrl ++ basename


baseUrl :: String
baseUrl = "https://raw.githubusercontent.com/words/moby/master/"


maybeDownload :: GenericPackageDescription -> (String, Bool, String) -> IO ()
maybeDownload g (basename, yesGZip, sha256) = doDownload >> verify yesGZip g basename sha256
 where
  doDownload :: IO ()
  doDownload = unlessM (doesFileExist $ getFilePath g basename) $ do
    printf "Downloading %s...\n" basename
    hFlush stdout
    download yesGZip g basename


download :: Bool -> GenericPackageDescription -> String -> IO ()
download yesGZip g basename = do
  rsp <- join (httpLbs <$> parseRequest url <*> newTlsManager)
  case statusCode (responseStatus rsp) of
    200  -> B.writeFile outFile ((if yesGZip then GZip.compress else id) $ responseBody rsp)
    code -> throwString ( "[RESP_CODE:"++show code++"] Failed to get " ++ basename ++ "\n" ++ manualDownloadMsg g basename)
  where
    outFile = getFilePath g basename ++ (if yesGZip then ".gz" else "")
    url = getUrl basename


verify :: Bool -> GenericPackageDescription -> FilePath -> String -> IO ()
verify yesGZip g basename hash = do
  computed <- show . sha256 <$> B.readFile filePath
  when (hash /= computed) $ throwString $ unlines
    [ "Incorrect checksum for " ++ filePath
    , "expected " ++ hash
    , "computed " ++ computed
    , manualDownloadMsg g basename
    ]
  where
    sha256 :: B.ByteString -> Hash.Digest Hash.SHA256
    sha256 = Hash.hashlazy

    filePath = dataDir (packageDescription g) ++ basename ++ (if yesGZip then ".gz" else "")


-- | File names relative to 'baseUrl' and their sha256.
fileInfos :: sha256 ~ String => yesGZip ~ Bool => [ (FilePath, yesGZip, sha256) ]
fileInfos = [
    ( "words.txt"
    , False
    , "63fcdcec2efe34414825945c7837f4381c7df7344ae34fdf1e8368eda19d59f3"
    ) ,
    ( "words.txt"
    , True
    , "c5eac0612486e3d09c0415a42084fb46cc08d2dff6df30744c4ad7e22f241236"
    ) ]


manualDownloadMsg :: GenericPackageDescription -> String -> String
manualDownloadMsg g basename = mainmsg ++ extramsg ++ "\n"
 where
  mainmsg = unwords
    [ "=> Please download"
    , getUrl basename
    , "and put it in your"
    , dataDir . packageDescription $ g
    , "folder."
    ]

  extramsg = if HS.null otherFiles then "" else
    "\n=> Other files to consider downloading:\n"
    ++ unlines (("   - " ++) . getUrl <$> HS.toList otherFiles)

  allFiles :: Set String
  allFiles = HS.fromList (fmap (\(x, _, _) -> x) fileInfos)

  otherFiles :: Set String
  otherFiles = HS.filter (/= basename) allFiles

