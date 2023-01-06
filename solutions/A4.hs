module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )
import Data.List (intercalate)
-- *** Assignment 4-1 *** --

-- Q#01

_HEADER_ =  " " ++ formatLine (map show _RANGE_)

-- Q#02

-- showSquares :: Square -> [String]
showSquares xs = map show xs


-- Q#03

dropFirstCol board_x= map tail board_x 

-- Q#04

dropLastCol board_x= map init board_x
-- 

--Q#05

formatRows xs = map (\x -> "_|_" ++ intercalate ("_|_") (showSquares x) ++ "_|_") xs
-- Q#06
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ _ [] = False
isWinningLine_ p line = null (filter (\x -> p/=x) line)


-- *** Assignment 4-2 *** --

-- Q#07

-- isWinningLine = undefined

-- Q#08

hasWon = undefined

-- Q#09

getGameState = undefined


playMove = undefined

-- Q#10

-- prependRowIndices = undefined

-- Q#11

formatBoard = undefined