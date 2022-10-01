module Anagram (anagramsFor) where
import Data.Char (toLower)

distinct :: (Eq a) => [a] -> [a] -> [a]
distinct [] results = results
distinct (item:remaining) results
  | item `elem` results = distinct remaining results
  | otherwise = distinct remaining (results ++ [item])

matchAllOnce :: (Eq a) => [a] -> [a] -> Bool
matchAllOnce [] _ = True
matchAllOnce (x:xs) candidate
  | x `notElem` candidate = False
  | otherwise = matchAllOnce xs (takeWhile (/= x) candidate ++ tail (dropWhile (/= x) candidate))

isAnagram :: String -> String -> Bool
isAnagram x y
  | length word /= length candidate = False
  | length wordSet /= length candidateSet = False
  | word == candidate = False
  | otherwise = matchAllOnce word candidate
  where word = map toLower x
        candidate = map toLower y
        wordSet = distinct word []
        candidateSet = distinct candidate []

foo :: String -> [String] -> [String] -> [String]
foo _ [] results = results
foo word (candidate:candidates) results
  | isAnagram word candidate = foo word candidates (results ++ [candidate])
  | otherwise                = foo word candidates results

anagramsFor :: String -> [String] -> [String]
anagramsFor word candidates = foo word candidates []