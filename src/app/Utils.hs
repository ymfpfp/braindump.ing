module Utils where

import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (dropWhileEnd)

slug :: String -> String
slug =
  map (\c -> if c == ' ' then '-' else c)
    . filter (\c -> isAlphaNum c || c == ' ')
    . map toLower
    . trimSpaces

trim :: (Char -> Bool) -> String -> String
trim f = dropWhileEnd f . dropWhile f

trimSpaces :: String -> String
trimSpaces = trim isSpace
