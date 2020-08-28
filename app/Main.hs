module Main where

import Config
import Commands
import Search

main :: IO ()
main = do
  repo <- searchRepoByPod "JYVBaseView"
  putStrLn repo
