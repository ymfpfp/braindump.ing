module Utils where

import Data.Char (isAlphaNum, isSpace, toLower, toUpper)
import Data.List (dropWhileEnd)

capitalize :: String -> String
capitalize = unwords . map upperFirst . words . map dashToSpace
  where
  upperFirst [] = []
  upperFirst (c:cs) = toUpper c : cs

dashToSpace :: Char -> Char
dashToSpace c = if c == ' ' then '-' else c

slug :: String -> String
slug =
  map dashToSpace
    . filter (\c -> isAlphaNum c || c == ' ')
    . map toLower
    . trimSpaces

trim :: (Char -> Bool) -> String -> String
trim f = dropWhileEnd f . dropWhile f

trimSpaces :: String -> String
trimSpaces = trim isSpace
