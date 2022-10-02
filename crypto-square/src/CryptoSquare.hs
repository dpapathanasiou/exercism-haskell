module CryptoSquare (encode) where
import Data.Char (isAlphaNum, toLower)

appendSpaces :: String -> Int -> String
appendSpaces s target = if length s == target then s else appendSpaces (s ++ " ") target

splitIntoRows :: String -> Int -> [String] -> [String]
splitIntoRows [] _ [] = []
splitIntoRows s w acc
  | null s = init acc ++ [appendSpaces (last acc) w]
  | otherwise = splitIntoRows (drop w s) w (acc ++ [take w s])

transpose :: [String] -> [String] -> [String]
transpose rows acc
  | all null rows = acc
  | otherwise = transpose (map tail rows) (acc ++ [map head rows])

joinOn :: [String] -> Char -> String -> String
joinOn [] _ result = result
joinOn (x:xs) c acc = joinOn xs c plus
  where plus = if null acc then x else acc ++ [c] ++ x

encode :: String -> String
encode original = crypto
  where
    normalized = map toLower (filter isAlphaNum original)
    len = fromIntegral (length normalized) :: Float
    rows = splitIntoRows normalized (ceiling (sqrt len)) []
    crypto = joinOn (transpose rows []) ' ' ""
