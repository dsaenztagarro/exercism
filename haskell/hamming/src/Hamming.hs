module Hamming (distance) where
import           Data.Monoid

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs == length ys = Just $ getSum $ mconcat $ map diff (zip xs ys)
  | otherwise = Nothing
  where diff (x, y) = if x /= y then Sum 1 else Sum 0
