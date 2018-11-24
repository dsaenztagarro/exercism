module Phone (number) where

import           Data.Char (digitToInt, isNumber)
import qualified Data.Text as T

number :: String -> Maybe String
number xs | invalidLength = Nothing
          | internationalCountryCode /= 1 = Nothing
          | firstDigit areaCode < 2 = Nothing
          | firstDigit exchangeCode < 2 = Nothing
          | otherwise = Just $ T.unpack numbers10
          where numbers = T.filter isNumber $ T.pack xs
                invalidLength = T.length numbers < 10 || T.length numbers > 11
                internationalCountryCode = if T.length numbers == 10 then 1 else digitToInt $ T.head numbers
                numbers10 = T.reverse $ T.take 10 $ T.reverse numbers
                areaCode = T.take 3 numbers10
                exchangeCode = T.take 3 $ T.drop 3 numbers10
                firstDigit ys = digitToInt $ T.head ys
