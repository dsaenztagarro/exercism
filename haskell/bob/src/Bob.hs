module Bob (responseFor) where

import           Data.Char       (isAlphaNum, isLetter, isUpper)
import           Data.List.Extra (notNull)

responseFor :: String -> String
responseFor xs
  | not $ isSentence xs         = "Fine. Be that way!"
  | isQuestion xs && isShout xs = "Calm down, I know what I'm doing!"
  | isQuestion xs               = "Sure."
  | isShout xs                  = "Whoa, chill out!"
  | otherwise                   = "Whatever."
  where isSentence xs = notNull xs && ((any isAlphaNum xs) || isQuestion xs)
        isShout xs = (any isLetter xs) && (all isUpper $ filter isLetter xs)
        isQuestion xs = isQuestion' $ reverse xs
          where isQuestion' [] = False
                isQuestion' (x:xs) | x == '?' = True
                                   | x == ' ' = isQuestion' xs
                                   | otherwise = False
