module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p [] = []
discard p (x:xs) = if p x
                   then discard p xs
                   else x : discard p xs

keep :: (a -> Bool) -> [a] -> [a]
keep p [] = []
keep p (x:xs) = if p x
                then x : keep p xs
                else keep p xs
