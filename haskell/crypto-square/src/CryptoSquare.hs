module CryptoSquare (encode) where

import           Data.Char (isLetter, isNumber, toLower)

encode :: String -> String
encode xs = unwords $ map getRow [0..y-1]
  where text = normalize xs
        (x, y) = dimensions text
        getRow i = spacesUntil x $ getRowChars text y i

normalize :: String -> String
normalize = foldr step []
  where step x xs | isLetter x = toLower x : xs
                  | isNumber x = x : xs
                  | otherwise = xs

dimensions :: String -> (Int, Int)
dimensions xs = (floor val, ceiling val)
  where val = sqrt $ fromIntegral (length xs) :: Float

spacesUntil :: Int -> String -> String
spacesUntil n xs = xs ++ replicate (n - length xs) ' '

getRowChars :: String -> Int -> Int -> String
getRowChars text numRows numRow = map fst $ filter belongsToRow charsWithIndex
  where charsWithIndex = zip text [0..]
        belongsToRow (_,i) = (i - numRow) `mod` numRows == 0
