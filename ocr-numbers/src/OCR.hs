module OCR (convert) where

encode :: [Char] -> [Int]
encode = map (\x -> if x == '_' then 2 else if x == '|' then 1 else 0)

decode :: [[Int]] -> Char
decode n
  | n == [[0,2,0],[1,0,1],[1,2,1],[0,0,0]] = '0'
  | n == [[0,0,0],[0,0,1],[0,0,1],[0,0,0]] = '1'
  | n == [[0,2,0],[0,2,1],[1,2,0],[0,0,0]] = '2'
  | n == [[0,2,0],[0,2,1],[0,2,1],[0,0,0]] = '3'
  | n == [[0,0,0],[1,2,1],[0,0,1],[0,0,0]] = '4'
  | n == [[0,2,0],[1,2,0],[0,2,1],[0,0,0]] = '5'
  | n == [[0,2,0],[1,2,0],[1,2,1],[0,0,0]] = '6'
  | n == [[0,2,0],[0,0,1],[0,0,1],[0,0,0]] = '7'
  | n == [[0,2,0],[1,2,1],[1,2,1],[0,0,0]] = '8'
  | n == [[0,2,0],[1,2,1],[0,2,1],[0,0,0]] = '9'
  | otherwise = '?'

groupByThree :: [[a]] -> [[a]] -> [[a]]
groupByThree xs acc
  | null xs = acc
  | all null xs = acc
  | otherwise = groupByThree (map (drop 3) xs) (acc ++ map (take 3) xs)

decodeByFour :: [[Int]] -> [Char] -> [Char]
decodeByFour xs a
  | null xs = a
  | all null xs = a
  | otherwise = decodeByFour (drop 4 xs) (a ++ [decode (take 4 xs)])

processDigit :: [[Char]] -> [Char]
processDigit line = decoded
  where grouper = groupByThree (map encode line) []
        decoded = decodeByFour grouper []

processInput :: [[Char]] -> [Char] -> [Char]
processInput xs a
  | null xs = a
  | all null xs = a
  | otherwise = processInput remaining (a ++ processDigit (take 4 xs) ++ separator)
  where remaining = drop 4 xs
        separator = if null remaining then "" else ","

convert :: String -> String
convert xs = processor
  where processor = processInput (lines xs) []
