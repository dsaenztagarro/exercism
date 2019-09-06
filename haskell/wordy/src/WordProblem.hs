{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module WordProblem (answer) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Either.Combinators
import           Data.String             (fromString)

data Operator = Addition | Substraction | Multiplication | Division deriving Show

type Operand = Integer

data Question = Question { number     :: Operand,
                           operations :: [(Operator, Operand)] } deriving Show

answer :: String -> Maybe Integer
answer text = parseQuestion text >>= return . result

parseQuestion :: String -> Maybe Question
parseQuestion text = rightToMaybe $ parseOnly questionParser (fromString text)

questionParser :: Parser Question
questionParser = do
  string "What is "
  number <- operandParser
  operations <- many operationParser
  string "?"
  return $ Question number operations

operatorParser :: Parser Operator
operatorParser =
      (string " plus " >> return Addition)
  <|> (string " minus " >> return Substraction)
  <|> (string " multiplied by " >> return Multiplication)
  <|> (string " divided by " >> return Division)

operandParser :: Parser Operand
operandParser = signed decimal

operationParser :: Parser (Operator, Operand)
operationParser = do
  operator' <- operatorParser
  operand' <- operandParser
  return (operator', operand')

result :: Question -> Integer
result question = foldl runOperation (WordProblem.number question) (operations question)
  where runOperation n (operator, operand) = case operator of
                                               Addition -> n + operand
                                               Substraction -> n - operand
                                               Multiplication -> n * operand
                                               Division -> n `div` operand
