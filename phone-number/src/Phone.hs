module Phone (number) where
import Data.Char (isDigit)

isValidAreaExchangeCode :: String -> Bool
isValidAreaExchangeCode n = head n `elem` ['2' .. '9']

number :: String -> Maybe String
number xs
  | len == 10 && isValidAreaExchangeCode (take 3 num) && isValidAreaExchangeCode (take 3 (drop 3 num)) = Just num
  | len == 11 && take 1 num == "1" && isValidAreaExchangeCode (drop 1 num) && isValidAreaExchangeCode (take 3 (drop 4 num)) = Just (drop 1 num)
  | otherwise = Nothing
  where num = filter isDigit xs
        len = length num