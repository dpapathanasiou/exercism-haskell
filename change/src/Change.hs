module Change (findFewestCoins) where
import Data.List (sortBy)

greedy :: [Integer] -> Integer -> Integer -> [Integer] -> Maybe [Integer]
greedy [] _ _ [] = Nothing
greedy [] target amount change
  | target == amount = Just change
  | otherwise = Nothing
greedy (x:xs) target amount change
  | x + amount <= target = greedy (x:xs) target (x + amount) (x : change)
  | otherwise = greedy xs target amount change

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0  = Nothing
  | target == 0 = Just []
  | otherwise = greedyAlgo
  where coinsDesc  = sortBy (flip compare) coins
        greedyAlgo = greedy coinsDesc target 0 []
