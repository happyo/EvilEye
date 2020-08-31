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

haha :: String -> IO ()
haha content = do
  -- print $ parse (many lockDependency >> manyTill anyChar eof) "" content
  print $ parse parserLockDepencies "" content

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

parserLockDepencies :: Parser [(String, [String])]
parserLockDepencies = do
  string "PODS:"
  many $ char ' '
  eol
  dependencies <- many lockDependency
  manyTill anyChar $ string "DEPENDENCIES:"
  manyTill anyChar eof
  return dependencies


lockDependency :: Parser (String, [String])
lockDependency = do
  parent <- lockParentPodLine
  childs <- option [] $ many (try lockChildPodLine)
  return (parent, childs)

lockParentPodLine :: Parser String
lockParentPodLine = do
  string "  -"
  many $ char ' '
  optional $ try $ char '\"'
  parent <- many validChar
  many $ char ' '
  optional $ try $ lockVersion
  optional $ try $ char '\"'
  optional $ try $ char ':'
  eol
  return parent

lockChildPodLine :: Parser String
lockChildPodLine = do
  string "    -"
  many $ char ' '
  optional $ try $ char '\"'
  child <- many validChar
  many $ char ' '
  optional $ try $ lockVersion
  optional $ try $ char '\"'
  eol
  return child

validChar :: Parser Char
validChar = alphaNum <|> oneOf "/_-=~|+."

lockVersion :: Parser String
lockVersion = do
  char '('
  version <- manyTill anyChar $ char ')'
  optional $ try (char ':')
  return version

eol :: Parser Char
eol = char '\n'
