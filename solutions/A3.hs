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
isColEmpty [] _ = False
isColEmpty (x:xs) 0 = x == E
isColEmpty (x:xs) i = i>0 && isColEmpty xs (i-1)

-- Q#05
dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol (x:xs) = drop 1 x : dropFirstCol xs




dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (x:xs) = init x : dropLastCol xs
-- Q#06

getDiag1 :: Board  -> Line
getDiag1 [] = []
getDiag1 (x:xs) = head x : getDiag1 (dropFirstCol xs)



getDiag2 :: Board  -> Line
getDiag2 [] = []
getDiag2 (x:xs) =  last x : getDiag2 (dropLastCol xs)


getAllLines :: Board -> [Line]
getAllLines [] = []
getAllLines x = x ++ transpose x ++ [getDiag1 x] ++ [getDiag2 x]
-- *** Assignment 3-2 ***

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine = undefined

-- Q#10

isValidMove = undefined