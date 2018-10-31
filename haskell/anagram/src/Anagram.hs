module Anagram ( anagramsFor ) where

import           Data.Char (toLower)
import           Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter isAnagram
  where
    word' = sort $ map toLower word
    isAnagram = (== word') . sort . map toLower
