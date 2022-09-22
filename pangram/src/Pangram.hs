module Pangram (isPangram) where

alphabetLower :: String
alphabetLower = "abcdefghiklmnopqrstvxyz"

alphabetUpper :: String
alphabetUpper = "ABCDEFGHIKLMNOPQRSTVXYZ"

areCharsInText :: String -> String -> String -> Bool
areCharsInText [] [] _ = True
areCharsInText (l:ls) (u:us) a
   | l `notElem` a && u `notElem` a = False
   | otherwise = areCharsInText ls us a

isPangram :: String -> Bool
isPangram = areCharsInText alphabetLower alphabetUpper
