module WordCount (wordCount) where
import Data.Char (toLower, isAlphaNum)

contractions :: [String]
contractions = ["'m", "'re", "'s", "'ve", "'ll", "'d", "'t"]

removePuncExceptContractions :: String -> String
removePuncExceptContractions input
  | suffix `elem` contractions = filter isAlphaNum prefix ++ suffix
  | otherwise = filter isAlphaNum (prefix ++ suffix)
  where
    prefix = takeWhile (/= '\'') input
    suffix = dropWhile (/= '\'') input

counter :: [String] -> [(String, Int)] -> [(String, Int)]
counter [] result = result
counter (x:xs) acc = counter xs (other ++ [(x, count + 1)])
  where
    prior = filter (\(w, _ ) -> w == x) acc
    other = filter (\(w, _ ) -> w /= x) acc
    count = if null prior then 0 else sum $ map snd prior

splitInput :: String -> [String]
splitInput s = words (map (\x -> if isAlphaNum x || x == '\'' then x else ' ') s)

wordCount :: String -> [(String, Int)]
wordCount xs = counter processedWords []
  where processedWords = map (removePuncExceptContractions . map toLower) (splitInput xs)
