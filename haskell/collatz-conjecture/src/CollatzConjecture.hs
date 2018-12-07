module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz x | x < 1 = Nothing
          | otherwise = Just $ snd $ step (x, 0)

step :: (Integer, Integer) -> (Integer, Integer)
step (1, n) = (1, n)
step (x, n) = step (y, n + 1) where y = if even x
                                        then x `div` 2
                                        else 3 * x + 1
