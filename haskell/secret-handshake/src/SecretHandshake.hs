module SecretHandshake (handshake) where

import           Data.Bits (testBit)

handshake :: Int -> [String]
handshake n = if testBit n 4
              then reverse xs
              else xs
  where xs = map messages $ filter (testBit n) [0..3]

messages :: Int -> String
messages 0 = "wink"
messages 1 = "double blink"
messages 2 = "close your eyes"
messages 3 = "jump"
