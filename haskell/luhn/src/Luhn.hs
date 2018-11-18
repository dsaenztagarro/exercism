module Luhn (isValid) where

import           Data.Char (digitToInt, intToDigit, isNumber)

isValid :: String -> Bool
isValid xs | length xs < 2 || xs == " 0" = False
           | otherwise = sum `mod` 10 == 0
            where sum = foldl (+) 0 $ doubleEven $ toIntList $ cleaned
                  cleaned = filter isNumber xs
                  toIntList = map (fromIntegral.digitToInt)

doubleEven :: [Integer] -> [Integer]
doubleEven xs = map step $ zip (reverse xs) [1..]
  where step (num, pos)  | isOdd pos = num
                         | num > 4 = num * 2 - 9
                         | otherwise = num * 2

isOdd :: Integer -> Bool
isOdd n = n `mod` 2 /= 0
