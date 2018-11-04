module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock
  { hour   :: Int
  , minute :: Int } deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock hour' min'
  where hourFromMin = min `div` 60
        hour' = (hour + hourFromMin) `mod` 24
        min' = min `mod` 60

toString :: Clock -> String
toString clock = twoDigits (hour clock) ++ ":" ++ twoDigits (minute clock)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour' min' clock = fromHourMin (hour' + hour clock) (min' + minute clock)

twoDigits :: Int -> String
twoDigits x | x < 10 = '0' : show x
            | otherwise = show x
