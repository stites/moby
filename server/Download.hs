{-# LANGUAGE TypeFamilies #-}
module Download where

import Control.Exception.Safe (throwString)
import Control.Monad (unless, when, join)
import Control.Monad.Extra (unlessM)
import Data.HashSet (HashSet)
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
import System.FilePath ((</>), normalise)
import System.Directory (doesFileExist)
import Text.Printf (printf)
import Network.HTTP.Client (httpLbs, parseRequest, responseStatus, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.HashSet as HS
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Lazy as B
import qualified Codec.Compression.GZip as GZip


fetchFiles :: FilePath -> IO ()
fetchFiles fp = mapM_ (maybeDownload fp) fileInfos


getFilePath :: FilePath -> FilePath -> Bool -> FilePath
getFilePath fp basename isgz = normalise $ fp ++ "/" ++ basename ++ (if isgz then ".gz" else "")


getUrl :: FilePath -> String
getUrl basename = baseUrl ++ basename


baseUrl :: String
baseUrl = "https://raw.githubusercontent.com/words/moby/master/"


maybeDownload :: FilePath -> (String, Bool, String) -> IO ()
maybeDownload fp (basename, yesGZip, sha256) = doDownload >> verify yesGZip fp basename sha256
 where
  doDownload :: IO ()
  doDownload = unlessM (doesFileExist $ getFilePath fp basename yesGZip) $ do
    putStrLn $  "Downloading " ++ basename ++ "..."
    download yesGZip fp basename


download :: Bool -> FilePath -> String -> IO ()
download yesGZip fp basename = do
  rsp <- join (httpLbs <$> parseRequest url <*> newTlsManager)
  case statusCode (responseStatus rsp) of
    200  -> do
      print (200, outFile, yesGZip)
      B.writeFile outFile ((if yesGZip then GZip.compress else id) $ responseBody rsp)

    code -> throwString
      $ "[RESP_CODE:"++show code++"] "
      ++"Failed to get " ++ basename
      ++ "\n" ++ manualDownloadMsg fp basename
  where
    outFile = getFilePath fp basename yesGZip
    url = getUrl basename


verify :: Bool -> FilePath -> FilePath -> String -> IO ()
verify yesGZip fp basename hash = do
  computed <- show . sha256 <$> B.readFile filePath
  when (hash /= computed) $ throwString $ unlines
    [ "Incorrect checksum for " ++ filePath
    , "expected " ++ hash
    , "computed " ++ computed
    , manualDownloadMsg fp basename
    ]
  where
    sha256 :: B.ByteString -> Hash.Digest Hash.SHA256
    sha256 = Hash.hashlazy

    filePath = getFilePath fp basename yesGZip


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


manualDownloadMsg :: FilePath -> String -> String
manualDownloadMsg fp basename = mainmsg ++ extramsg ++ "\n"
 where
  mainmsg = unwords
    [ "=> Please download"
    , getUrl basename
    , "and put it in your"
    , fp -- dataDir . packageDescription $ g
    , "folder."
    ]

  extramsg = if HS.null otherFiles then "" else
    "\n=> Other files to consider downloading:\n"
    ++ unlines (("   - " ++) . getUrl <$> HS.toList otherFiles)

  allFiles :: HashSet String
  allFiles = HS.fromList (fmap (\(x, _, _) -> x) fileInfos)

  otherFiles :: HashSet String
  otherFiles = HS.filter (/= basename) allFiles

