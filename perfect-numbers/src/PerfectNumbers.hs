module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factors :: Int -> [Int]
factors n
  | n < 1 = []
  | otherwise = [ i | i <- [1 .. n], n `mod` i == 0 ]

classify :: Int -> Maybe Classification
classify n
  | n < 1 = Nothing
  | n == aliquot = Just Perfect
  | n <  aliquot = Just Abundant
  | otherwise = Just Deficient
  where aliquot = sum $ filter (/= n) (factors n)