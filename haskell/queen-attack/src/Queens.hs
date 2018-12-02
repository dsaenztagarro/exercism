module Queens (boardString, canAttack) where

import           Data.List (elem, intersperse)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ map (printRank white black) [0..7]

printRank :: Maybe (Int, Int) -> Maybe (Int, Int) -> Int -> String
printRank white black n = intersperse ' ' [ print (n, i) | i <- [0..7]]
  where print = printSquare white black

printSquare :: Maybe (Int, Int) -> Maybe (Int, Int) -> (Int, Int) -> Char
printSquare white black (x, y) | white == Just (x, y) = 'W'
                               | black == Just (x, y) = 'B'
                               | otherwise = '_'

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA queenB = elem queenB $ moves queenA

moves :: (Int, Int) -> [(Int, Int)]
moves (y, x) = sameRank ++ sameFile ++ diagonal1 ++ diagonal2 ++ diagonal3 ++ diagonal4
  where sameFile = [(y', x) | y' <- [0..7]]
        sameRank = [(y, x') | x' <- [0..7]]
        diagonal1 = [(y - i, x + i) | i <- [1..6], y - i >= 0, x + i <= 7]
        diagonal2 = [(y + i, x - i) | i <- [1..6], y + i <= 7, x - i >= 0]
        diagonal3 = [(y - i, x - i) | i <- [1..6], y - i >= 0, x - i >= 0]
        diagonal4 = [(y + i, x + i) | i <- [1..6], y + i <= 7, x + i <= 7]

