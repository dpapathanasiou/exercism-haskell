module SumOfMultiples (sumOfMultiples) where

dividesCleanly :: Integer-> Integer -> Bool
dividesCleanly _ 0 = False
dividesCleanly x y = x `rem` y == 0

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum $ filter (`divideByAny` factors) [1 .. limit-1]
  where
    divideByAny x = any (x `dividesCleanly`)
