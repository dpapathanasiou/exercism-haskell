module Change (findFewestCoins) where

{-
-- greedy algorithm fails with unusual sets of coins:

change with Lilliputian Coins FAILED [1]

Failures:

  test/Tests.hs:19:33: 
  1) change with Lilliputian Coins
       expected: Just [4,4,15]
        but got: Just [1,1,1,20]


greedy :: [Integer] -> Integer -> Integer -> [Integer] -> Maybe [Integer]
greedy [] _ _ [] = Nothing
greedy [] target amount change
  | target == amount = Just change
  | otherwise = Nothing
greedy (x:xs) target amount change
  | x + amount <= target = greedy (x:xs) target (x + amount) (x : change)
  | otherwise = greedy xs target amount change
-}

import Data.Array ( array, (!) )

dynamic :: [Integer] -> Integer -> Maybe (Integer, [Integer])
dynamic coins target = find c target
  where c = length coins - 1
        r = array ((0,0), (c, target)) [((index, amount), compute index amount) | index <- [0 .. c], amount <- [0 .. target]]
        find index amount
          | index  < 0  = Nothing
          | amount < 0  = Nothing
          | amount == 0 = Just (0, [])
          | otherwise = r ! (index, amount)
        compute index amount
          | coinValue > amount = find (index - 1) amount
          | otherwise = case (find index (amount - coinValue), find (index - 1) amount) of
                          (Just (countL, resultL), Just (countR, resultR)) -> if countL + 1 <= countR then Just (countL + 1, coinValue : resultL) else Just (countR, resultR)
                          (Nothing, Just (count, result)) -> Just (count, result)
                          (Just (count, result), Nothing) -> Just (count + 1, coinValue : result)
                          (Nothing, Nothing) -> Nothing
          where coinValue = coins !! index

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins = 
  case dynamic coins target of
    Just (_, result) -> Just result
    Nothing          -> Nothing

  {-
  | target < 0  = Nothing
  | target == 0 = Just []
  | otherwise = greedyAlgo
  where coinsDesc  = sortBy (flip compare) coins -- needs import Data.List (sortBy)
        greedyAlgo = greedy coinsDesc target 0 []
  -}