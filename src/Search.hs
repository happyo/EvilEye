module Search where

import Commands
import Config
import System.Directory.PathWalk
import Control.Monad.Trans
import Control.Monad.Writer
import System.FilePath

searchRepoByPod :: String -> IO String
searchRepoByPod pod = do
  config <- readConfig
  goRepoDir config
  repoName <- pathWalkAccumulate "." $ \root dirs _ -> do
    if (isPodInRepo pod dirs)
      then return root
      else return ""
  return $ takeBaseName repoName

-- searchGitUrlByPod :: String -> IO String
-- searchGitUrlByPod pod = do
--   config <- readConfig
--   goRepoDir config


isPodInRepo :: String -> [String] -> Bool
isPodInRepo pod repoPods = elem pod repoPods
