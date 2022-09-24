module CollatzConjecture (collatz) where

foo :: Integer -> Integer -> Integer
foo n counter
  | n == 1 = counter
  | even n = foo (n `div` 2) (counter + 1)
  | otherwise = foo ((3 * n) + 1) (counter + 1)

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | otherwise = Just (foo n 0)