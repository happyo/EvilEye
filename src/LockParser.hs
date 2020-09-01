module LockParser where

import Text.Parsec
import Text.Parsec.String
import Data.Char (isDigit
                 )

data Pod = Pod {
  name :: String,
  version :: String
} deriving (Show, Eq)

data PodDependency = PodDependency {
  parentPod :: Pod,
  childPods :: [Pod]
} deriving (Show, Eq)

haha :: String -> IO ()
haha content = do
  print $ parse parserLockPodInfo "" content

getLockPodDependencies :: String -> [(Pod, [Pod])]
getLockPodDependencies contents = case parse parserLockDepencies "" contents of
  Left _ -> []
  Right r -> r

getLockPodInfo :: String -> [(String, [(String, String)])]
getLockPodInfo contents = case parse parserLockPodInfo "" contents of
  Left _ -> []
  Right r -> r

parserLockDepencies :: Parser [(Pod, [Pod])]
parserLockDepencies = do
  string "PODS:"
  many $ char ' '
  eol
  dependencies <- many lockDependency
  manyTill anyChar eof
  return dependencies

parserLockPodInfo :: Parser [(String, [(String, String)])]
parserLockPodInfo = do
  manyTill anyChar $ try $ string "EXTERNAL SOURCES:"
  many $ char ' '
  eol
  pods <- many podInfo
  -- manyTill anyChar $ string "CHECKOUT OPTIONS:"
  manyTill anyChar eof
  return $ filter specifiedVersionPod pods

specifiedVersionPod :: (String, [(String, String)]) -> Bool
specifiedVersionPod (_, kvs) = null (filter (\(k, _) -> k == "branch" || k == "commit") kvs)

podInfo :: Parser (String, [(String, String)])
podInfo = do
  string "  "
  podName <- manyTill anyChar (char ':')
  many $ char ' '
  eol
  infos <- many $ try lineInfo
  return (podName, infos)


lineInfo :: Parser (String, String)
lineInfo = do
  string "    :"
  tag <- manyTill anyChar (char ':')
  many $ char ' '
  optional $ try $ char '\"'
  value <- many validChar
  optional $ try $ char '\"'
  many $ char ' '
  eol
  return (tag, value)


lockDependency :: Parser (Pod, [Pod])
lockDependency = do
  parent <- lockParentPodLine
  childs <- option [] $ many (try lockChildPodLine)
  return (parent, childs)

lockParentPodLine :: Parser Pod
lockParentPodLine = do
  string "  -"
  many $ char ' '
  optional $ try $ char '\"'
  parent <- many validChar
  many $ char ' '
  version <- option "" $ try $ lockVersion
  optional $ try $ char '\"'
  optional $ try $ char ':'
  eol
  return $ Pod parent version

lockChildPodLine :: Parser Pod
lockChildPodLine = do
  string "    -"
  many $ char ' '
  optional $ try $ char '\"'
  child <- many validChar
  many $ char ' '
  version <- option "" $ try $ lockVersion
  optional $ try $ char '\"'
  eol
  return $ Pod child version

validChar :: Parser Char
validChar = alphaNum <|> oneOf "/_-=~|+.@:"

lockVersion :: Parser String
lockVersion = do
  char '('
  version <- manyTill anyChar $ char ')'
  optional $ try (char ':')
  return version

eol :: Parser Char
eol = char '\n'
