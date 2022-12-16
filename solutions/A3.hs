module A3 where

import A1
import A2

import Data.List (transpose)
import Control.Monad (forM)

-- *** Assignment 3-1 ***

-- Q#01
showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs


_HEADER_ =  " " ++ formatLine (showInts _RANGE_)

-- Q#02
showSquares :: [Square] -> [String]
showSquares []      = []
showSquares (x:xs)  = show x : showSquares xs


-- Q#03
formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (x:xs) = formatLine(showSquares x) : formatRows xs
-- ++ "," ++ formatRows xs

-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty [E,E,E] _ = True 
isColEmpty _ -1 = False 
isColEmpty (x:xs) i = (x == E)

-- Q#05

dropFirstCol = undefined


dropLastCol = undefined

-- Q#06

getDiag1 = undefined


getDiag2 = undefined


getAllLines = undefined

-- *** Assignment 3-2 ***

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine = undefined

-- Q#10

isValidMove = undefined