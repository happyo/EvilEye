module Commands where

import System.Process
import Config
import System.Directory
import System.FilePath
import Control.Exception
import LockParser
import PodParser

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

-- 根据config里的配置下载所有相关pod，并切到对应分支
updateDependencyCommand :: IO ()
updateDependencyCommand = do
  config <- readConfig
  putStrLn "--- config ---"
  print config
  contents <- readFile "Podfile.lock"
  let needUpdatePods = updatePods config
      lockPodDependencies = getLockPodDependencies contents
  putStrLn "--- dependencies ---"
  print lockPodDependencies
  putStrLn "--- Asso ---"
  print $ getAssociatedPods needUpdatePods lockPodDependencies


getAssociatedPods :: [String] -> [(String, [String])] -> [String]
getAssociatedPods needUpdates podDependencies = map (\podName ->
                                                       findParentPod podName podDependencies) needUpdates

isDependency :: String -> (String, [String]) -> Bool
isDependency podName dependency = elem podName $ snd dependency

findParentPod :: String -> [(String, [String])] -> String
findParentPod _ [] = ""
findParentPod podName (d:ds) = if isDependency podName d
  then fst d
  else findParentPod podName ds
