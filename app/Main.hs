module Main where

import Config
import Commands
import Search
import PodParser
import LockParser

main :: IO ()
main = do
  updateDependencyCommand
  -- print $ getPodDependencies contents
