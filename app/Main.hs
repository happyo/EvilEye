module Main where

import Config
import Commands
import Search
import PodParser

main :: IO ()
main = do
  contents <- readFile "haha.podspec"
  haha contents
