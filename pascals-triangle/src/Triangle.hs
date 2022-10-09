module Triangle (rows) where

factorial :: Int -> Int
factorial 0 = 1
factorial n = foldr (*) 1 [1 .. n]

binomial :: Int -> Int -> Int
binomial n k = factorial n `div` (factorial k * factorial (n - k))

-- The entry in the nth row and kth column of Pascal's triangle is denoted: binomial n k

rows :: Int -> [[Integer]]
rows x = [[toInteger (binomial r c) | c <- [0 .. r]] | r <- [0 .. (x - 1)]]
