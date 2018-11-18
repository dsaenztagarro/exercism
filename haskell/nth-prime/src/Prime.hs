module Prime (nth) where

nth :: Int -> Maybe Integer
nth n | n <= 0 = Nothing
      | otherwise = Just (fromIntegral $ primes !! n)
          where primes = 1:[ x | x <- [2..], isPrime x]
                isPrime i = factors i == [1]
                  where factors z = [ x | x <- [1..z `div` 2], z `mod` x == 0]
