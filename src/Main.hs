{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Monoid
import           Data.Text (Text)
import           GHC.Generics
import           Options.Generic
import           Prelude hiding (FilePath)
import           Shelly

---

data Args = Args { areas :: String <?> "Path to a JSON file defining areas to convert." }
  deriving (Generic, ParseRecord)

-- | An OSM Extract area to process.
data Area = Area { name :: Text, countries :: [Text] } deriving (Generic, FromJSON, Show)

-- | Location of `osm2orc` executable (java program).
osm2orc :: FilePath
osm2orc = "/home/colin/code/java/osm2orc/build/distributions/osm2orc-0.3.1/bin/osm2orc"

-- | Where to upload the converted `.orc` files to.
s3 :: Text
s3 = "s3://vectortiles/orc/"

root :: Text
root = "download.geofabrik.de/"

-- | Given an `Area`, form the URLs of where its PBF files should be located.
urls :: Area -> [Text]
urls (Area n cs) = map (\c -> n <> "/" <> c <> "-latest.osm.pbf") cs

-- | Download a given `.osm.pbf` file to the filesystem.
download :: Text -> IO FilePath
download = undefined

-- | Convert a downloaded `.osm.pbf` to a `.orc`.
convert :: FilePath -> IO FilePath
convert = undefined

-- | Upload a converted `.orc` to S3.
upload :: FilePath -> IO ()
upload = undefined

work :: [Area] -> IO ()
work as = mapM_ print as

main :: IO ()
main = do
  Args (Helpful as) <- getRecord "Draenor - Batch convert OSM PBF to ORC and upload to S3."
  bytes <- B.readFile as
  either putStrLn (\as' -> work as' >> putStrLn "Done.") $ eitherDecode bytes
