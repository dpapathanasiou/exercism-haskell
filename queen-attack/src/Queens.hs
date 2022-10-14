module Queens (boardString, canAttack) where

boardMin :: Int
boardMax :: Int
(boardMin, boardMax) = (0, 7)

generateSquare :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [Char]
generateSquare (r, c) (wx, wy) (bx, by)
  | r == wx && c == wy = 'W' : trailer
  | r == bx && c == by = 'B' : trailer
  | otherwise = '_' : trailer
  where trailer = if c == boardMax then ['\n'] else [' ']

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = concat [(\(x, y) -> 
  generateSquare (x, y) (wx, wy) (bx, by))
  (i, j) | i <- [boardMin .. boardMax], j <- [boardMin .. boardMax]]
  where
    out = (boardMin - 1, boardMin - 1)
    (wx, wy) = case white of
        Nothing -> out
        Just xy -> xy
    (bx, by) = case black of
        Nothing -> out
        Just xy -> xy

computeDiagonals :: (Int, Int) -> [(Int, Int)]
computeDiagonals (x, y) = filter (\(i, j) -> i >= boardMin && i <= boardMax && j >= boardMin && j <= boardMax)
  [(x+z, y+z) | z <- [1 .. boardMax]] ++ 
  [(x-z, y-z) | z <- [1 .. boardMax]] ++ 
  [(x+z, y-z) | z <- [1 .. boardMax]] ++ 
  [(x-z, y+z) | z <- [1 .. boardMax]] 

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (ax, ay) (bx, by)
  | ax == bx || ay == by = True
  | (ax, ay) `elem` computeDiagonals (bx, by) = True
  | otherwise = False
