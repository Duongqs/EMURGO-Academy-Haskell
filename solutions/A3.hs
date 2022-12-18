module A3 where

import A1
import A2

import Data.List (transpose)
import Control.Monad (forM, join)
import System.Posix.Internals (puts)
import Text.ParserCombinators.ReadP (char)

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
-- isColEmpty :: Row -> Int -> Bool
-- isColEmpty [] _ = False
-- isColEmpty (x:xs) 0 = x == E
-- isColEmpty (_:xs) i = i>0 && isColEmpty xs (i-1)

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty (x:xs) 0 = x == E
isColEmpty (_:xs) i | i>0           = isColEmpty xs (i-1)
                    | otherwise     = False

-- Q#05
dropFirstCol :: Board -> Board
dropFirstCol []      = []
dropFirstCol ([]:xs) = [] : dropFirstCol xs
dropFirstCol (x:xs)  = tail x : dropFirstCol xs


dropLastCol :: Board -> Board
dropLastCol []      = []
dropLastCol ([]:xs) = [] : dropLastCol xs
dropLastCol (x:xs)  = init x : dropLastCol xs
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

putSquare :: Player -> Board -> Move -> Board 
putSquare _ [] (_,_) = []
putSquare p (bx:bxs) (row,col) = go 0 (bx:bxs)
    where 
        go :: Int -> Board -> Board
        go _ [] = []
        go i (bx:bxs)
            | i == row = replaceSquareInRow p col bx : bxs
            | otherwise =  bx : go (i+1) bxs
-- Q#08

prependRowIndices :: [String] -> [String]
prependRowIndices [] =[]
prependRowIndices x = go (zip ['A'..] x )
    where 
        go :: [(Char,String)] ->[String]
        go []           = []
        go ((a1,b1):ax) = ([a1] ++ b1) : go ax

-- Q#09

isWinningLine :: Player -> Line -> Bool
isWinningLine _ [] = False
isWinningLine p l = go True l
    where 
        go :: Bool -> Line -> Bool
        go acc []       =  acc
        go acc (x:xs) =  p==x && go acc xs

-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove [] _ = False
isValidMove x (i,j)  = isMoveInBounds (i,j) && go True x i
        where 
            go :: Bool -> Board -> Int -> Bool
            go _ [] _            = False 
            go acc (x:_) 0       = isColEmpty x j && acc
            go acc (_x:xs) y     =   go acc xs (y-1)

    -- Otherwise False