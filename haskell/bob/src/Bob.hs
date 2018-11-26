{-# LANGUAGE OverloadedStrings #-}

module Bob (responseFor) where

import           Data.Char (isLetter, isLower, isSpace)
import           Data.Text (Text)
import qualified Data.Text as T

responseFor :: Text -> Text
responseFor xs
  | isSilence             = "Fine. Be that way!"
  | isQuestion && isShout = "Calm down, I know what I'm doing!"
  | isQuestion            = "Sure."
  | isShout               = "Whoa, chill out!"
  | otherwise             = "Whatever."
  where isSilence  = T.all isSpace xs
        isQuestion = "?" `T.isSuffixOf` T.stripEnd xs
        isShout    = T.any isLetter xs && T.all (not . isLower) xs
