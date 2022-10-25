module Hamming (distance) where

hamming :: String -> String -> Int -> Maybe Int
hamming [] [] a = Just a
hamming [] _ _ = Nothing
hamming _ [] _ = Nothing
hamming (x:xs) (y:ys) a
  | x == y = hamming xs ys a
  | otherwise = hamming xs ys (a + 1)

distance :: String -> String -> Maybe Int
distance xs ys = hamming xs ys 0