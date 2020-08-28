{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Config where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.ByteString as B
import System.Directory
import System.FilePath

data Config = Config {
  workDir :: String,
  reposFolderName :: String,
  privateRepos :: [String]
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .:   "workDir"       <*>
    v .:   "reposFolderName"       <*>
    v .:   "privateRepos"
  parseJSON _ = fail "Expected Object for Config value"

readConfig :: IO Config
readConfig = do
  homePath <- getHomeDirectory
  configString <- B.readFile $ homePath </> configLocation
  res <- Y.decodeThrow configString
  return (res :: Config)

configLocation :: FilePath
configLocation = ".EvilEye/config.yaml"
