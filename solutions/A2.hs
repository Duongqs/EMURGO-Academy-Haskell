{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)

-- *** Assignment 2-1 *** --

-- Q#01
promptPlayer :: Player -> String
--promptPlayer x = "Player " ++ show(x) ++ "'s turn: enter a row and column position (ex. A1)"
promptPlayer x = concat["Player ",show(x),"'s turn: enter a row and column position (ex. A1)"]


-- Q#02

_RANGE_ = [1 .. _SIZE_]

-- Q#03

isDigit :: Char -> Bool
isDigit x = x `elem` ['0' .. '9']


readDigit :: Char -> Int
readDigit x = if isDigit(x) then read([x])
                else -1


-- Q#04

_EMPTY_ROW_ = replicate _SIZE_ E


_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
isTied :: Board -> Bool
isTied x = E `notElem` concat(x)


_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06
indexRowStrings :: [String] -> [(Char,String)]
indexRowStrings x = zip ['A'..] x

-- Q#07
formatLine :: [String] -> String
formatLine x = _SEP_++intercalate (_SEP_) (x) ++_SEP_

-- *** Assignment 2-2 *** --

-- Q#08

isMoveInBounds = undefined

-- Q#09

stringToMove = undefined

-- Q#10

replaceSquareInRow = undefined