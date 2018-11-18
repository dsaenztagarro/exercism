module CryptoSquare (encode) where

import           Data.Char       (isAlphaNum, toLower)
import           Data.List       (transpose)
import           Data.List.Split (chunksOf)

encode :: String -> String
encode xs = unwords . transpose $ chunksOf sqSize cleaned
  where cleaned = filter isAlphaNum . map toLower $ xs
        sqSize = ceiling doubleSqSize
          where doubleSqSize :: Double
                doubleSqSize = sqrt . fromIntegral $ length cleaned
