module PodParser where

import Text.Parsec
import Text.Parsec.String
import Control.Monad

parseGit :: Parser String
parseGit = do
  skipUntil (string ":git")
  whitespace
  string "=>"
  whitespace
  char '\''
  w <- many $ (noneOf "\'")
  char '\''
  return w

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

skipUntil :: Parser String -> Parser String
skipUntil p = try p <|> (anyChar >> skipUntil p)

getPodDependencies :: String -> [String]
getPodDependencies content = filter (\s -> not (s == "")) $ map getLineDependency (lines content)

getLineDependency :: String -> String
getLineDependency line = case (parse (try commentLine <|> parserDependency) "" line) of
  Left _ -> ""
  Right r -> r

commentLine :: Parser String
commentLine = do
  whitespace
  char '#'
  manyTill anyChar newline
  return ""

parserDependency :: Parser String
parserDependency = do
  skipUntil (string ".dependency")
  whitespace
  char '\''
  w <- many $ (noneOf "\'")
  char '\''
  return w

