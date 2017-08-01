{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
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

data Args = Args { areas :: String <?> "Path to a JSON file defining areas to convert."
                 , cache :: FilePath <?> "Path to a directory for storing the downloaded .pbf files."
                 , threads :: Int <?> "Number of CPU threads to use."
                 } deriving (Generic, ParseRecord)

-- | The runtime environment.
data Env = Env { _cache :: FilePath
               , _downloaded :: M.Map Text FilePath
               , _countries :: TChan (Text, Text)
               , _toConvert :: TChan FilePath
               , _toUpload :: TChan Text }

-- | An OSM Extract area to process.
data Area = Area { name :: Text, countries :: [Text] } deriving (Generic, FromJSON)

-- | Location of `osm2orc` executable (java program).
osm2orc :: FilePath
osm2orc = "/home/colin/code/java/osm2orc/build/distributions/osm2orc-0.3.1/bin/osm2orc"

-- | Where to upload the converted `.orc` files to.
s3 :: Text
s3 = "s3://vectortiles/orc/"

-- | Given an `Area` name and a country, form the URL of where its PBF file should be located.
url :: Text -> Text -> Text
url n c = "download.geofabrik.de/" <> n <> "/" <> c <> "-latest.osm.pbf"

-- | If not already present on the filesystem, download a given `.osm.pbf` file.
download :: Env -> IO ()
download env = do
  next <- atomically . tryReadTChan $ _countries env
  case next of
    Nothing -> pure ()  -- All downloads complete.
    Just (n, c) -> getNext n c >>= atomically . writeTChan (_toConvert env) >> download env
  where getNext n c = case M.lookup c (_downloaded env) of
          Just fp -> TIO.putStrLn (c <> " already downloaded.") >> pure fp
          Nothing -> do
            let fp = _cache env </> c <.> "osm.pbf"
            TIO.putStrLn $ "Downloading " <> c <> " data..."
            shelly $ run_ "wget" ["-q", "-O", toTextIgnore fp, url n c]
            pure fp

convert :: Env -> IO ()
convert env = do
  next <- atomically $ (,) <$> tryPeekTChan (_countries env) <*> tryReadTChan (_toConvert env)
  case next of
    (Nothing, Nothing) -> pure ()  -- All conversions complete.
    (Just _, Nothing) -> threadDelay 1000000 >> convert env
    (_, Just pbf) -> do
      let pbf' = toTextIgnore pbf
          orc  = toTextIgnore $ _cache env </> basename pbf <.> "orc"
      TIO.putStrLn $ "Converting " <> pbf'
      shelly $ run_ osm2orc [pbf', orc]
      atomically $ writeTChan (_toUpload env) orc
      convert env

-- | Upload a converted `.orc` to S3.
upload :: Env -> IO ()
upload env = do
  next <- atomically $ (,,) <$> tryPeekTChan (_countries env) <*> tryPeekTChan (_toConvert env) <*> tryReadTChan (_toUpload env)
  case next of
    (Nothing, Nothing, Nothing) -> pure ()  -- All uploads complete.
    (_, _, Just orc) -> do
      TIO.putStrLn $ "Uploading " <> orc
      shelly $ run_ "aws" ["s3", "cp", orc, s3, "--quiet"]
      TIO.putStrLn $ "Uploading " <> orc <> " complete!"
      upload env
    _ -> threadDelay 1000000 >> upload env

work :: Env -> Int -> [Area] -> IO ()
work env t as = do
  atomically . mapM_ (writeTChan (_countries env)) $ as >>= \(Area n cs) -> map (\c -> (n, c)) cs
  void . runConcurrently $ (,,)
    <$> Concurrently (replicateConcurrently_ t $ download env)
    <*> Concurrently (replicateConcurrently_ t $ convert env)
    <*> Concurrently (replicateConcurrently_ t $ upload env)

main :: IO ()
main = do
  Args (Helpful as) (Helpful c) (Helpful t) <- getRecord "Draenor - Batch convert OSM PBF to ORC and upload to S3."
  pbfs <- shelly $ mkdir_p c >> ls c  -- All already downloaded pbf files.
  bytes <- B.readFile as
  (cc, fc, tc) <- (,,) <$> newTChanIO <*> newTChanIO <*> newTChanIO
  let pbfs' = M.fromList . map (\p -> (toTextIgnore $ basename p, p)) $ filter (hasExt "pbf") pbfs
      env = Env c pbfs' cc fc tc
  case decode bytes of
    Nothing -> putStrLn "There was a parsing error in your JSON file."
    Just as' -> work env t as' >> putStrLn "Done."
