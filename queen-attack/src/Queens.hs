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

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (ax, ay) (bx, by)
  | ax == bx || ay == by = True
  | abs (ax - bx) == abs (ay - by) = True
  | otherwise = False
