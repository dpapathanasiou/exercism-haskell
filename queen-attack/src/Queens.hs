module Queens (boardString, canAttack) where

boardMin :: Int
boardMax :: Int
(boardMin, boardMax) = (0, 7)

board :: [Int]
board = [boardMin .. boardMax]

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [ unwords [ generateSquare i j | j <- board] | i <- board]
  where
    generateSquare x y
      | Just (x, y) == white = "W"
      | Just (x, y) == black = "B"
      | otherwise            = "_"

computeDiagonals :: (Int, Int) -> [(Int, Int)]
computeDiagonals (x, y) = filter (\(i, j) -> i >= boardMin && i <= boardMax && j >= boardMin && j <= boardMax)
  (concat [[(x+z, y+z), (x-z, y-z), (x+z, y-z), (x-z, y+z)] | z <- [1 .. boardMax]])

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (ax, ay) (bx, by)
  | ax == bx || ay == by = True
  | (ax, ay) `elem` computeDiagonals (bx, by) = True
  | otherwise = False
