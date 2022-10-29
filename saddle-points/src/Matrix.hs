module Matrix (saddlePoints) where

import Data.Array ( (!), indices, Array )

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = filter (\(i, j) -> matrix ! (i, j) == maxInRow i && matrix ! (i, j) == minInCol j) coords
    where
        coords = indices matrix
        maxInRow r = maximum (map (matrix !) (filter (\(x, _) -> x == r) coords))
        minInCol c = minimum (map (matrix !) (filter (\(_, y) -> y == c) coords))
