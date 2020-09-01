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
  let dTwo = getLockPodDependencies fileTwo
      podsOne = podsByDependencies $ getLockPodDependencies fileOne
      podsTwo = podsByDependencies dTwo
      podsInfo = getLockPodInfo fileTwo
  mapM_ print $ filter (\(a, b) -> not (sameVersion a b)) $ same podsOne podsTwo
  putStrLn "--- Pod Info ---"
  print podsInfo
  putStrLn "--- All ---"
  print $ getAll ["SDWebImage"] dTwo podsInfo
 

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
getPodB _ [] = Pod "" ""
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
  contents <- readFile "B.lock"
  let needUpdatePods = updatePods config
      lockPodDependencies = getLockPodDependencies contents
      podsInfo = getLockPodInfo contents
  putStrLn "--- dependencies ---"
  print lockPodDependencies
  putStrLn "--- Pods Info ---"
  print podsInfo

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

getAll :: [String] -> [(Pod, [Pod])] -> [PodInfo] -> [String]
getAll u ds ps = nub $ getAllIter u ds ps []

getAllIter :: [String] -> [(Pod, [Pod])] -> [PodInfo] -> [String] -> [String]
getAllIter [] _ _ r = r
getAllIter u ds ps r = getAllIter current ds ps (r ++ current)
  where current = getAssociatedPods u ds ps

getAssociatedPods :: [String] -> [(Pod, [Pod])] -> [PodInfo] -> [String]
getAssociatedPods needUpdates podDependencies ps = filter (\s -> (not (null s)))
  $ map name $ needUpdates >>= (\podName ->
                                  if isSpecified podName ps
                                  then findParentPod podName podDependencies ps
                                  else [])

isDependency :: String -> (Pod, [Pod]) -> Bool
isDependency podName dependency = elem podName $ map name $ snd dependency

specifiedVersionPod :: PodInfo -> Bool
specifiedVersionPod p = null (filter (\(k, _) -> k == "branch" || k == "commit") kvs)
  where kvs = _infos p

isSpecified :: String -> [PodInfo] -> Bool
isSpecified _ [] = True
isSpecified pName (p:ps) = if pName == _podInfoName p
  then specifiedVersionPod p
  else isSpecified pName ps

findParentPod :: String -> [(Pod, [Pod])] -> [PodInfo] -> [Pod]
findParentPod _ [] _ = []
findParentPod podName (d:ds) ps = if isDependency podName d
  then (fst d) : (findParentPod podName ds ps)
  else findParentPod podName ds ps
