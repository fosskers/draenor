{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent.Async
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import           Filesystem.Path (basename)
import           GHC.Generics
import           Options.Generic
import           Prelude hiding (FilePath)
import           Shelly

---

data Args = Args { areas :: String <?> "Path to a JSON file defining areas to convert."
                 , cache :: FilePath <?> "Path to a directory for storing the downloaded .pbf files."
                 } deriving (Generic, ParseRecord)

-- | The runtime environment.
data Env = Env FilePath (M.Map Text FilePath)

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
download :: Env -> Text -> Text -> Sh FilePath
download (Env cpath pbfs) n c = case M.lookup c pbfs of
  Just fp -> echo (c <> " already downloaded.") >> pure fp
  Nothing -> do
    let fp = cpath </> c <.> "osm.pbf"
    echo $ "Downloading " <> c <> " data..."
    run_ "wget" ["-q", "-O", toTextIgnore fp, url n c]
    pure fp

-- | Convert a downloaded `.osm.pbf` to a `.orc`.
convert :: Env -> FilePath -> Sh Text
convert (Env cpath _) pbf = echo ("Converting " <> pbf') >> run_ osm2orc [pbf', orc] >> pure orc
  where orc = toTextIgnore $ cpath </> basename pbf <.> "orc"
        pbf' = toTextIgnore pbf

-- | Upload a converted `.orc` to S3.
upload :: Text -> Sh ()
upload orc = run_ "aws" ["s3", "cp", orc, s3]

convertArea :: Env -> Area -> IO ()
convertArea env a = forConcurrently_ (countries a) $ \c ->
  shelly (download env (name a) c >>= convert env >> pure ()) --upload)

work :: Env -> [Area] -> IO ()
work env as = mapConcurrently_ (convertArea env) as

main :: IO ()
main = do
  Args (Helpful as) (Helpful c) <- getRecord "Draenor - Batch convert OSM PBF to ORC and upload to S3."
  pbfs <- shelly $ mkdir_p c >> ls c  -- All already downloaded pbf files.
  bytes <- B.readFile as
  let env = Env c $ M.fromList $ map (\p -> (toTextIgnore $ basename p, p)) pbfs
  case decode bytes of
    Nothing -> putStrLn "There was a parsing error in your JSON file."
    Just as' -> work env as' >> putStrLn "Done."
