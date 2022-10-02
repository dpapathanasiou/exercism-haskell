module Luhn (isValid) where
import Data.Char (isNumber, digitToInt)

doubleUnderNine :: Int -> Int
doubleUnderNine d = if double > 9 then double - 9 else double
  where double = d * 2

luhnDigits :: [String] -> [Int] -> [Int]
luhnDigits [] result = result
luhnDigits (x:xs) acc = luhnDigits xs (acc ++ [doubleUnderNine (digitToInt (head x))] ++ [digitToInt (last x)])

pairs :: String -> [String] -> [String]
pairs [] acc = acc
pairs s acc = pairs (take p s) (acc ++ [drop p s])
  where p = length s - 2

isValid :: String -> Bool
isValid n 
  | length numbers < 2 = False
  | otherwise = sum (luhnDigits (pairs numbers []) []) `mod` 10 == 0
  where numbers = filter isNumber n