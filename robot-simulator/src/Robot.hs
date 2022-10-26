module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Coordinates = Coordinates {
    x :: Integer
  , y :: Integer
} deriving (Eq, Show)

data Robot = Robot {
    c :: Coordinates
  , b :: Bearing
} deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing = b

coordinates :: Robot -> (Integer, Integer)
coordinates robot = (x coords, y coords)
  where coords = c robot

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction (x, y) = Robot (Coordinates x y) direction

turn :: Bearing -> Char -> Bearing
turn bearing direction
  | bearing == North && direction == 'R' = East
  | bearing == North && direction == 'L' = West
  | bearing == South && direction == 'R' = West
  | bearing == South && direction == 'L' = East
  | bearing == East  && direction == 'R' = South
  | bearing == East  && direction == 'L' = North
  | bearing == West  && direction == 'R' = North
  | bearing == West  && direction == 'L' = South
  | otherwise = bearing

updateCoords :: Coordinates -> Bearing -> Char -> Coordinates
updateCoords coords bearing direction 
  | direction == 'A' && bearing == North = Coordinates r (c + 1)
  | direction == 'A' && bearing == South = Coordinates r (c - 1)
  | direction == 'A' && bearing == East  = Coordinates (r + 1) c
  | direction == 'A' && bearing == West  = Coordinates (r - 1) c
  | otherwise = coords
  where r = x coords
        c = y coords

move :: Robot -> String -> Robot
move robot [] = robot
move robot (r:rs) = move newBot rs
  where coords = updateCoords (c robot) (b robot) r
        newBot = mkRobot (turn (b robot) r) (x coords, y coords)
