module Commands where

import System.Process
import Config
import System.Directory
import System.FilePath
import Control.Exception
import LockParser
import PodParser
import Data.List

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

-- 比较两个lock文件中版本不同的库
compareDiffVersionPods :: IO ()
compareDiffVersionPods = do
  fileOne <- readFile "A.lock"
  fileTwo <- readFile "B.lock"
  let podsOne = podsByDependencies $ getLockPodDependencies fileOne
      podsTwo = podsByDependencies $ getLockPodDependencies fileTwo
  mapM_ print $ filter (\(a, b) -> not (sameVersion a b)) $ same podsOne podsTwo

same :: [Pod] -> [Pod] -> [(Pod, Pod)]
same (x:xs) ys = if x `elemPod` ys
                 then (x, getPodB x ys) : (same xs ys)
                 else same xs ys
same [] _ = []
same _ [] = []

sameVersion :: Pod -> Pod -> Bool
sameVersion one two = version one == version two

elemPod :: Pod -> [Pod] -> Bool
elemPod _ [] = False
elemPod pod (x:xs) = if (name pod == name x)
  then True
  else elemPod pod xs

getPodB :: Pod -> [Pod] -> Pod
getPodB pod (x:xs) = if (name pod == name x)
  then x
  else getPodB pod xs

podsByDependency :: (Pod, [Pod]) -> [Pod]
podsByDependency (parent, cs) = parent : cs

podsByDependencies :: [(Pod, [Pod])] -> [Pod]
podsByDependencies [] = []
podsByDependencies (x:xs) = [(fst x)] ++ podsByDependencies xs

-- 根据config里的配置下载所有相关pod，并切到对应分支
updateDependencyCommand :: IO ()
updateDependencyCommand = do
  config <- readConfig
  putStrLn "--- config ---"
  print config
  -- contents <- readFile "te.lock"
  contents <- readFile "Podfile.lock"
  let needUpdatePods = updatePods config
      lockPodDependencies = getLockPodDependencies contents
  putStrLn "--- dependencies ---"
  print lockPodDependencies

  -- putStrLn "--- Asso ---"
  -- let a1 = getAssociatedPods needUpdatePods lockPodDependencies
  --     a2 = getAssociatedPods a1 lockPodDependencies
  --     a3 = getAssociatedPods a2 lockPodDependencies
  -- putStrLn "---"
  -- print $ a1
  -- print $ a2
  -- print $ a3
  -- putStrLn "---"
  -- -- print $ getAll needUpdatePods lockPodDependencies

getAll :: [String] -> [(String, [String])] -> [String]
getAll u ds = nub $ getAllIter u ds []

getAllIter :: [String] -> [(String, [String])] -> [String] -> [String]
getAllIter [] _ r = r
getAllIter u ds r = getAllIter current ds (r ++ current)
  where current = getAssociatedPods u ds

getAssociatedPods :: [String] -> [(String, [String])] -> [String]
getAssociatedPods needUpdates podDependencies = filter (\s -> (not (null s)))
  $ needUpdates >>= (\podName ->
                       findParentPod podName podDependencies)

isDependency :: String -> (String, [String]) -> Bool
isDependency podName dependency = elem podName $ snd dependency

findParentPod :: String -> [(String, [String])] -> [String]
findParentPod _ [] = []
findParentPod podName (d:ds) = if isDependency podName d
  then (fst d) : (findParentPod podName ds)
  else findParentPod podName ds
