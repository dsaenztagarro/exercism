{-# LANGUAGE OverloadedStrings #-}

module Bob (responseFor) where

import           Data.Char (isLetter)
import qualified Data.Text as T

responseFor :: T.Text -> T.Text
responseFor xs
  | isSilence             = "Fine. Be that way!"
  | isQuestion && isShout = "Calm down, I know what I'm doing!"
  | isQuestion            = "Sure."
  | isShout               = "Whoa, chill out!"
  | otherwise             = "Whatever."
  where cleaned    = T.strip xs
        isSilence  = T.null cleaned
        isQuestion = T.isSuffixOf "?" cleaned
        isShout    = (T.any isLetter cleaned) && (T.toUpper cleaned) == cleaned
