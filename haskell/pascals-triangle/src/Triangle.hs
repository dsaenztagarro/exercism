module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x = map row [0..x-1]
  where row n = [combination n k | k <- [0..n]]

combination :: Int -> Int -> Integer
combination n k = toInteger $ factorial n `div` ((factorial k) * (factorial $ n - k))

factorial :: Int -> Int
factorial n = product [1..n]
