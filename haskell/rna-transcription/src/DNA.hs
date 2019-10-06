module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = traverse trans xs

trans :: Char -> Either Char Char
trans 'G' = Right 'C'
trans 'C' = Right 'G'
trans 'T' = Right 'A'
trans 'A' = Right 'U'
trans x   = Left x
