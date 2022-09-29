module Acronym (abbreviate) where

import Data.Char ( isAlpha, isLower, isUpper, toUpper ) 

foo :: (Char, String) -> Char -> (Char, String)
foo (prior, acc) x
  | prior == '\'' && x == 's'        = (x, acc)
  | not (isAlpha prior) && isAlpha x = (x, acc ++ [toUpper x])
  | isLower prior && isUpper x       = (x, acc ++ [x])
  | otherwise = (x, acc)

abbreviate :: String -> String
abbreviate = snd . foldl foo (' ', "")