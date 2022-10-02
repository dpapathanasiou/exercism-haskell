module Prime (nth) where

primes :: [Integer]
primes = sieve [2..]
  where
    sieve [] = []
    sieve (x:xs) = x : sieve [p | p <- xs, mod p x /= 0]

nth :: Int -> Maybe Integer
nth n
  | n > 0 = Just (primes !! (n - 1))
  | otherwise = Nothing