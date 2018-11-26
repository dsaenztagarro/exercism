module WordCount (wordCount) where

import           Data.Char (isAlphaNum, toLower)
import           Data.List (partition)

wordCount :: String -> [(String, Int)]
wordCount xs = count $ stripQuotations $ words $ normalize xs

count [] = []
count ws = [(h, length pxs)] ++ (count notpxs)
  where h = head $ ws
        (pxs, notpxs) = partition ((==) h) ws

normalize :: String -> String
normalize xs = map normalizeChar xs
  where normalizeChar x = if isAlphaNum x || isQuot x
                          then toLower x
                          else ' '

stripQuotations :: [String] -> [String]
stripQuotations xs = map doStrip xs
  where doStrip xs | isQuot $ head xs = init $ tail xs
                   | otherwise = xs

isQuot :: Char -> Bool
isQuot = (==) '\''
