module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock {
    hour   :: Int
  , minute :: Int
}
  deriving Eq

rolloverHour :: Int -> Int
rolloverHour hr
  | hrs == 24 = 0
  | hrs < 0   = 24 + hrs
  | otherwise = hrs
  where (_, hrs) = quotRem hr 24

rolloverTime :: Int -> Int -> (Int, Int)
rolloverTime h m = (rolloverHour hrs, mins)
  where (hours, minutes) = quotRem ((h * 60) + m) 60
        hrs
          | minutes == 60 = hours + 1
          | minutes < 0   = hours - 1
          | otherwise = hours
        mins
          | minutes == 60 = 0
          | minutes < 0   = 60 + minutes
          | otherwise = minutes

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock hrs mins
  where (hrs, mins) = rolloverTime h m

prePendZero :: Int -> String
prePendZero i = if i < 10 then "0" ++ show i else show i

toString :: Clock -> String
toString clock = hrs ++ ":" ++ mins
  where hrs  = prePendZero (hour clock)
        mins = prePendZero (minute clock)

addDelta :: Int -> Int -> Clock -> Clock
addDelta h m clock = fromHourMin hrs mins
  where (hrs, mins) = rolloverTime (hour clock + h) (minute clock + m)
