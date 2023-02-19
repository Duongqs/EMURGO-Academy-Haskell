module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)


-- *** Assignment 5-1 *** --

-- Q#01

printBoard :: Board -> IO ()
printBoard b = putStrLn $ formatBoard b

--printBoard x = print (formatBoard x)
-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"


printLogo :: IO ()
printLogo = readFile _LOGO_PATH_  >>= putStrLn 


-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen


firstPlayer :: IO Bool -> IO Player
firstPlayer rd =  getFirstPlayer <$> rd


-- Q#04

-- getMove = undefined
getMove :: Board  -> IO Move
getMove board =  putStrLn " please enter valid move" >> getLine >>= worker board
worker b input = if isValidMove b move then return move
                 else putStrLn "it is not valid move, please input again" 
                        >> getMove b
                 where move = stringToMove input



    
    -- do
    -- move <- getLine
    -- let realmove = stringToMove move
    -- if isValidMove board realmove
    --     then return realmove
    -- else putStrLn "it is not valid move, please input again" 
    -- >> getMove board


-- getMove' board = do
    -- move <- getLine
    -- getMovevalid move board
 
-- Q#05

-- play :: Board -> Player -> IO ()
-- play board player = do
--     when _DISPLAY_LOGO_  printLogo
--     printBoard board
--     putStrLn.promptPlayer $ player
--     move <- getMove board
--     let (gamestate, newboard) = playMove player board move 
--     if gamestate == In_progress 
--         then play newboard (switchPlayer  player)
--         else do printBoard newboard >>  putStrLn(showGameState gamestate)
-- *** Assignment 5-2 *** --

-- Q#07

printLogoDo :: IO ()
printLogoDo = do
    logo <- readFile _LOGO_PATH_
    putStrLn logo  
-- Q#08

firstPlayerDo :: IO Bool -> IO Player
firstPlayerDo rd =  do
    ramdon <- rd
    return (getFirstPlayer ramdon)

-- Q#09

getMoveDo :: Board  -> IO Move
getMoveDo board = do
    move <- getLine
    let realmove = stringToMove move
    if isValidMove board realmove
        then return realmove
    else putStrLn "it is not valid move, please input again"
         >> getMoveDo board

-- Q#10

playDo :: Board -> Player -> IO ()
playDo board player = do
    when _DISPLAY_LOGO_  printLogo
    printBoard board
    putStrLn.promptPlayer $ player
    move <- getMove board
    let (gamestate, newboard) = playMove player board move 
    if gamestate == In_progress 
        then play newboard (switchPlayer  player)
        else do printBoard newboard >>  putStrLn(showGameState gamestate)
-- *** Assignment 5-2 *** --