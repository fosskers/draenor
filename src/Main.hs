{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception (SomeException, catch)
import           Control.Monad (void)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Filesystem.Path (basename)
import           GHC.Generics
import           Options.Generic
import           Prelude hiding (FilePath)
import           Shelly

---

-- | Command-line arguments.
data Args = Args { areas   :: String   <?> "Path to a JSON file defining areas to convert."
                 , cache   :: FilePath <?> "Path to a directory for storing the downloaded .pbf files."
                 , osm2orc :: FilePath <?> "Path to an osm2orc executable."
                 , s3      :: Text     <?> "S3 Bucket/Key to upload to, like: s3://vectortiles/orc/"
                 , threads :: Int      <?> "Number of CPU threads to use."
                 } deriving (Generic, ParseRecord)

-- | The runtime environment.
data Env = Env { _args           :: Args
               , _downloaded     :: M.Map Text FilePath
               , _countries      :: TChan (Text, Text)
               , _toConvert      :: TChan FilePath
               , _toUpload       :: TChan Text
               , _downloadDone   :: TVar Int
               , _conversionDone :: TVar Int }

-- | An OSM Extract area to process.
data Area = Area { name :: Text, countries :: [Text] } deriving (Generic, FromJSON)

-- | Given an `Area` name and a country, form the URL of where its PBF file should be located.
url :: Text -> Text -> Text
url n c = "download.geofabrik.de/" <> n <> "/" <> c <> "-latest.osm.pbf"

-- | If not already present on the filesystem, download a given `.osm.pbf` file.
download :: Env -> IO ()
download env = do
  next <- atomically . tryReadTChan $ _countries env
  case next of
    Nothing -> atomically $ modifyTVar' (_downloadDone env) (+ 1)
    Just (n, c) -> getNext n c >>= atomically . writeTChan (_toConvert env) >> download env
  where getNext n c = case M.lookup c (_downloaded env) of
          Just fp -> TIO.putStrLn (c <> " already downloaded.") >> pure fp
          Nothing -> do
            let fp = (unHelpful . cache $ _args env) </> c <.> "osm.pbf"
            TIO.putStrLn $ "Downloading " <> c <> " data..."
            catch @SomeException
              (shelly (run_ "wget" ["-q", "-O", toTextIgnore fp, url n c]) >> pure fp)
              (const (threadDelay 5000000 >> TIO.putStrLn "Trying again..." >> getNext n c))

convert :: Env -> IO ()
convert env = do
  next <- atomically $ (,) <$> readTVar (_downloadDone env) <*> tryReadTChan (_toConvert env)
  case next of
    (n, Nothing) | n == (unHelpful . threads $ _args env) -> atomically $ modifyTVar' (_conversionDone env) (+ 1)
                 | otherwise -> threadDelay 1000000 >> convert env
    (_, Just pbf) -> do
      let pbf' = toTextIgnore pbf
          orc  = toTextIgnore $ (unHelpful . cache $ _args env) </> basename pbf <.> "orc"
      TIO.putStrLn $ "Converting " <> pbf'
      shelly $ run_ (unHelpful . osm2orc $ _args env) [pbf', orc]
      atomically $ writeTChan (_toUpload env) orc
      convert env

-- | Upload a converted `.orc` to S3.
upload :: Env -> IO ()
upload env = do
  next <- atomically $ (,) <$> readTVar (_conversionDone env) <*> tryReadTChan (_toUpload env)
  case next of
    (n, Nothing) | n == (unHelpful . threads $ _args env) -> pure ()  -- All uploads complete.
                 | otherwise -> threadDelay 1000000 >> upload env
    (_, Just orc) -> do
      TIO.putStrLn $ "Uploading " <> orc
      shelly $ run_ "aws" ["s3", "cp", orc, unHelpful . s3 $ _args env, "--quiet"]
      TIO.putStrLn $ "Uploading " <> orc <> " complete!"
      upload env

work :: Env -> [Area] -> IO ()
work env as = do
  atomically . mapM_ (writeTChan (_countries env)) $ as >>= \(Area n cs) -> map (\c -> (n, c)) cs
  void . runConcurrently $ (,,) <$> f download <*> f convert <*> f upload
  where f g = Concurrently . replicateConcurrently_ (unHelpful . threads $ _args env) $ g env

main :: IO ()
main = do
  args <- getRecord ".osm.pbf -> .orc, and upload to S3."
  pbfs <- shelly $ mkdir_p (unHelpful $ cache args) >> ls (unHelpful $ cache args)  -- Already downloaded pbf files.
  bytes <- B.readFile . unHelpful $ areas args
  (cc, fc, tc) <- (,,) <$> newTChanIO <*> newTChanIO <*> newTChanIO
  (dt, ct) <- (,) <$> newTVarIO 0 <*> newTVarIO 0
  let pbfs' = M.fromList . map (\p -> (toTextIgnore $ basename p, p)) $ filter (hasExt "pbf") pbfs
      env = Env args pbfs' cc fc tc dt ct
  case decode bytes of
    Nothing -> putStrLn "There was a parsing error in your JSON file."
    Just as' -> work env as' >> putStrLn "Done."
