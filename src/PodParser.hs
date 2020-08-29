module PodParser where

import Text.Parsec
import Text.Parsec.String
import Control.Monad

-- gitURL :: Parsec String () String
-- gitURL = do

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

haha :: String -> IO ()
haha content = do
  parseTest parseGit content

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

skipUntil :: Parser String -> Parser String
skipUntil p = try p <|> (anyChar >> skipUntil p)
