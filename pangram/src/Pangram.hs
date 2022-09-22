module Pangram (isPangram) where

import Data.Char

alphabet :: String
alphabet = ['a' .. 'z']

areCharsInText :: String -> String -> Bool
areCharsInText [] _ = True
areCharsInText (x:xs) a
   | x `notElem` a = False
   | otherwise = areCharsInText xs a

isPangram :: String -> Bool
isPangram text = areCharsInText alphabet (map toLower text)
