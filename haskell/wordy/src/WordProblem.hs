{-# LANGUAGE OverloadedStrings #-}
-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec
-- https://mmhaskell.com/blog/2017/2/6/applicatives-one-step-further
-- https://hackernoon.com/attoparsec-the-clarity-of-do-syntax-95bf47846855
-- https://mmhaskell.com/parsing-3
-- https://mmhaskell.com/parsing-2
module WordProblem (answer) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char            (digitToInt)
import           Data.Text            (pack)
import           Debug.Trace

newtype Problem = Problem { d1 :: Integer } deriving Show

numberParser :: Parser Problem
numberParser = do
  string "What is "
  d1 <- count 1 digit
  return (Problem (read d1 :: Integer))

sumParser :: Parser Problem
sumParser = do
  string "What is "
  d <- count 1 digit
  string " plus "
  dd <- count 1 digit
  return (Problem ((read d :: Integer) + (read dd :: Integer)))

expressionParser :: Parser Problem
expressionParser = sumParser <|> numberParser

-- problemParser :: Parser Problem
-- problemParser = do
--   string "What is "
--   d1 <- count 1 digit
--   return (Problem (read d1 :: Integer))

answer :: String -> Maybe Integer
answer problem = case parseResult of
    Left _error        -> trace ("attoparsec error: " ++ _error) Nothing
    Right (Problem d1) -> Just d1
  where
    parseResult = parseOnly expressionParser $ pack problem
