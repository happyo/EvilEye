module Search where

import Commands
import Config
import System.Directory.PathWalk
import Control.Monad.Trans
import Control.Monad.Writer

searchRepoByPod :: String -> IO String
searchRepoByPod pod = do
  config <- readConfig
  goRepoDir config
  repoName <- pathWalkAccumulate "." $ \root dirs _ -> do
    if (isPodInRepo pod dirs)
      then return root
      else return ""

  return repoName

  -- pathWalk "." $ \root dirs files -> do
  --     putStrLn root
  --     putStrLn $ "  dirs: " ++ show dirs
  --     putStrLn $ "  files: " ++ show files
  --     if (isPodInRepo pod dirs)
  --       then return root
  --       else


isPodInRepo :: String -> [String] -> Bool
isPodInRepo pod repoPods = elem pod repoPods
