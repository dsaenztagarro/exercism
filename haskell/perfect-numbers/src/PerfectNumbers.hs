module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n | n <= 0 = Nothing
classify n | diff == 0 = Just Perfect
           | diff > 0  = Just Abundant
           | diff < 0  = Just Deficient
           where diff = (aliquotSum n) - n
                 aliquotSum n = foldl (+) 0 (factors n)
                 factors n = [ j | j <- [1..(n `div` 2)], (n `mod` j) == 0 ]
