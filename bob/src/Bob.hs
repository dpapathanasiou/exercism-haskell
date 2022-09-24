module Bob (responseFor) where

import Data.Char ( isSpace )

getLetters :: String -> String
getLetters = filter (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'])

isShouting :: String -> Bool
isShouting text = all (`elem` ['A' .. 'Z']) text && not (null text)

responseFor :: String -> String
responseFor xs
   | null xs || all isSpace xs = "Fine. Be that way!"
   | isShouting request && punctuation == '?' = "Calm down, I know what I'm doing!"
   | punctuation == '?' = "Sure."
   | (isShouting request && punctuation == '!') || isShouting (getLetters xs) = "Whoa, chill out!"
   | otherwise = "Whatever."
   where punctuation = head (dropWhile isSpace (reverse xs))
         request = getLetters (init xs)

