{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module A1 where

import Data.Char (toUpper)
import Text.Read (Lexeme(String))

-- *** Assignment 1-1 *** --

-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex x = fromEnum (toUpper x ) - 65

-- Q#04
_INVALID_MOVE_ :: (Int , Int)
_INVALID_MOVE_  = (-1,-1)

-- Q#05
_SEP_  :: String
_SEP_ = "_|_"

-- *** Assignment 1-2 *** --

-- Q#06
data Square = X | O | E
        deriving (Show, Eq)


-- Q#07
data GameState = X_won | O_won | Tie | In_progress | Playing
        deriving (Show, Eq)


-- Q#08
type Player = Square

type Row = [Square]

type Line = [Square]

type Board = [Row]

type Move = (Int,Int)



-- Q#09
getFirstPlayer :: Bool -> Player
getFirstPlayer x = if x then X else O


getFirstPlayer_ x
        | x = X
        | not x = O

-- Q#10

showGameState :: GameState -> String
showGameState stateGame = case stateGame of
        X_won -> show ("the first player won")
        O_won -> show ("the second player won")
        Tie -> show ("the game is draw")
        In_progress -> show ("the game is still happening")
-- Q#11
switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
switchPlayer E = E


-- Q#12

showSquare :: Square -> String
showSquare x = case  x of
        X -> "X"
        O -> "O"
        E -> "_"

showSquare2 :: Square -> String
showSquare2 x = case  x of
        X -> show x
        O -> show x
        E -> "_"