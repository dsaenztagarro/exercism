module Acronym (abbreviate) where

import           Data.Char

keepChar :: (Char, String) -> Char -> (Char, String)
keepChar (l, xs) c
    | not (isAlpha l) && isAlpha c  = (c, xs++[toUpper c])
    | isLower l && isUpper c        = (c, xs++[toUpper c])
    | otherwise                     = (c, xs)

abbreviate :: String -> String
abbreviate = snd . foldl keepChar (' ',[])
