module Acronym (abbreviate) where

import Data.Char ( isSpace, isUpper, toUpper, isPunctuation, isLower )

foo :: String -> Char -> String -> String
foo [] _ acc = acc
foo (x:xs) prior acc
  | isUpper x && (isSpace prior || isLower prior) = foo xs x (acc ++ [x])
  | not (isSpace x) && not (isPunctuation x) && (isSpace prior || isPunctuation prior) = foo xs x (acc ++ [toUpper x])
  | otherwise = foo xs x acc

abbreviate :: String -> String
abbreviate xs = foo text ' ' ""
  where text = filter (/= '\'') xs