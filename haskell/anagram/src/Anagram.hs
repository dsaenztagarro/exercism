module Anagram (anagramsFor) where

import           Data.Char (toLower)
import           Data.List (deleteFirstsBy)
import           GHC.Char  (eqChar)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter $ isAnagram xs

isAnagram :: String -> String -> Bool
isAnagram xs ys
  | length xs /= length ys = False
  | lowercase xs == lowercase ys = False
  | otherwise = null $ deleteFirstsBy eqChar (lowercase xs) (lowercase ys)

lowercase :: String -> String
lowercase = map toLower
