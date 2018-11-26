module Pangram (isPangram) where

import           Data.Char (chr, toLower)
import qualified Data.Set  as S

isPangram :: String -> Bool
isPangram text = alphabet `S.intersection` sentence == alphabet
  where alphabet = S.fromList $ map chr [97..122]
        sentence = S.fromList $ map toLower text
