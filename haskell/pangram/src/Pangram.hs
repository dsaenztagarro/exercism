module Pangram (isPangram) where

import           Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = all charIsPresent ['a'..'z']
  where charIsPresent x = any ((==) x . toLower) text
