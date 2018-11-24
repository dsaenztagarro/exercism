module Bob (responseFor) where

import           Data.Char (isLetter)
import qualified Data.Text as T

responseFor :: String -> String
responseFor xs
  | isSilence             = "Fine. Be that way!"
  | isQuestion && isShout = "Calm down, I know what I'm doing!"
  | isQuestion            = "Sure."
  | isShout               = "Whoa, chill out!"
  | otherwise             = "Whatever."
  where cleaned    = T.strip $ T.pack xs
        isSilence  = T.null cleaned
        isQuestion = T.isSuffixOf (T.pack "?") cleaned
        isShout    = (T.any isLetter cleaned) && (T.toUpper cleaned) == cleaned
