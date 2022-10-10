module Triangle (rows) where

-- generate an infinite list of lists and take the requested amount

rows :: Int -> [[Integer]]
rows x = take x $ iterate nextRow [1]
    where nextRow priorRow = zipWith (+) (0 : priorRow) (priorRow ++ [0])