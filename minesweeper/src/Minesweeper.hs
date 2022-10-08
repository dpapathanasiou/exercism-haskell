module Minesweeper (annotate) where

space :: Char
space = ' '

mine :: Char
mine = '*'

getNeighbors :: [String] -> (Int, Int) -> [Char]
getNeighbors board (r, c) = [(\(x, y) ->
  (board !! x) !! y)
  (i, j) | i <- [r - 1 .. r + 1], i >= 0, i < length board,
           j <- [c - 1 .. c + 1], j >= 0, j < length (head board),
           (i, j) /= (r, c)]

computeNeighborSum :: Char -> [Char] -> String
computeNeighborSum char neighbors
  | char  == mine = [mine]
  | count == 0    = [space]
  | otherwise     = show count
  where count = sum $ map (\x -> if x == space then 0 else 1) neighbors

transpose :: [String] -> [String] -> [String]
transpose rows acc 
  | all null rows = acc
  | otherwise = transpose (map tail rows) (acc ++ [map head rows])

annotate :: [String] -> [String]
annotate [] = []
annotate board 
  | length board == 1 && head board == "" = [""]
  | otherwise = transpose (map (concatMap (\(x, y) -> computeNeighborSum ((board !! x) !! y) (getNeighbors board (x,y)))) coords) []
  where coords = [[(i,j) | i <- [0 .. length board - 1]] | j <- [0 .. length (head board) - 1] ]
