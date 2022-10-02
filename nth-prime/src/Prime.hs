module Prime (nth) where

sieve :: Integer -> [Integer]
sieve n = [toInteger x | x <- [2 .. n], and [x `mod` y /= 0 | y <- [2 .. (floor (sqrt (fromInteger x)))]]]

findNth :: Int -> Integer -> Integer
findNth target index
  | length primes >= target = last (take target primes)
  | otherwise = findNth target (index * 100) -- sieve is efficient, so jump an arbitrary large amount
  where primes = sieve index

nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = Just (findNth n 2)
