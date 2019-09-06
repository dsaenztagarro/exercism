module CollatzConjecture (collatz) where

import           Data.List (genericLength)

collatz :: Integer -> Maybe Integer
collatz x | x > 0 = Just $ genericLength (takeWhile (/= 1) $ iterate next x)
          | otherwise = Nothing

next :: Integer -> Integer
next x | even x = x `div` 2
       | otherwise = 3 * x + 1
