module Acronym (abbreviate) where

import           Data.Char (isUpper, toUpper)
import           Data.List (elemIndex)

abbreviate :: String -> String
abbreviate xs = concatMap letters $ words xs

letters :: String -> String
letters [] = []
letters ys
  | all isUpper ys = [head ys]
  | any isHyphen ys = concatMap letters $ splitOnHyphen ys
letters (y:ys) = toUpper y : filter isUpper ys

-- Assumption only 1 hyphen per word
splitOnHyphen :: String -> [String]
splitOnHyphen xs = [ys, tail zs]
  where (ys, zs) = break isHyphen xs

isHyphen :: Char -> Bool
isHyphen '-' = True
isHyphen _   = False
