module Commands where

import System.Process
import Config
import System.Directory
import System.FilePath
import Control.Exception

testCommand :: IO ()
testCommand = do
  config <- readConfig
  goRepoDir config
  mapM_ gitCloneCommand (privateRepos config)
  callCommand "ls"

gitCloneCommand :: String -> IO ()
gitCloneCommand url = do
  result <- try (callCommand $ "git clone " ++ url) :: IO (Either SomeException ())
  print result

goWorkDir :: Config -> IO ()
goWorkDir config = do
  homePath <- getHomeDirectory
  let workPath = homePath </> (workDir config)
  setCurrentDirectory workPath

goRepoDir :: Config -> IO ()
goRepoDir config = do
  homePath <- getHomeDirectory
  let repoPath = homePath </> (workDir config) </> (reposFolderName config)
  setCurrentDirectory repoPath
