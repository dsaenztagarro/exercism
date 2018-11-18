module Minesweeper (annotate, locationOfMines) where

import           Data.Char       (intToDigit)
import qualified Data.Foldable   as F
import           Data.List.Split (chunksOf)
import qualified Data.Set        as S

type Board = [Row]
type Row = String
type Square = (Integer, Integer)

annotate :: Board -> Board
annotate [] = []
annotate [""] = [""]
annotate board = chunksOf (length $ head board) $ map transform $ squares board
  where mines = locationOfMines board
        transform square | containsMine mines square = '*'
                         | n > 0 = intToDigit n
                         | otherwise = ' '
                          where n = hint mines square

locationOfMines :: Board -> [Square]
locationOfMines board = map fst $ filter (isMine.snd) $ zip (squares board) (concat board)

squares :: Board -> [Square]
squares board = [(x,y) | y <- [0..h], x <- [0..w]]
  where w = (fromIntegral $ length $ head board) - 1
        h = (fromIntegral $ length board) - 1

isMine :: Char -> Bool
isMine = (==) '*'

containsMine :: [Square] -> Square -> Bool
containsMine mines square = S.member square $ S.fromList mines

hint :: [Square] -> Square -> Int
hint mines point = S.size $ mines' `S.intersection` cellsAround'
  where mines' = S.fromList mines
        cellsAround' = S.fromList $ adjacentCells point

adjacentCells :: Square -> [Square]
adjacentCells (x, y) = filter isInsideBoard potentialCells
  where potentialCells = [(x-1, y-1), (x, y-1), (x+1, y-1),
                          (x-1, y),             (x+1, y),
                          (x-1, y+1), (x, y+1), (x+1, y+1)]
        isInsideBoard (x, y) = x >= 0 && y >= 0
