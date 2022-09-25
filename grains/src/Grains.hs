module Grains (square, total) where

squares :: [Integer]
squares = 0 : 1 : [x * 2 | x <- tail squares]

square :: Integer -> Maybe Integer
square n
  | n < 1 || n > 64 = Nothing
  | otherwise = Just (squares !! fromInteger n)

total :: Integer
total = sum $ take (64 + 1) squares
