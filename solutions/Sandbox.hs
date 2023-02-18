{-# LANGUAGE StrictData #-}
{-# LANGUAGE InstanceSigs #-}
module Sandbox where
import Text.Read (Lexeme(String, Char))
import Foreign (toBool)
import GHC.Enum (Enum)
import Data.List
---
half :: Int -> Double
half n = (fromIntegral n)/2.0
---

fizzBuzz :: Int -> String
fizzBuzz x =
    if (x `mod` 3 == 0 && x `mod` 5 == 0 ) then "FizzBuzz"
    else if (x `mod` 3 == 0 ) then "Fizz"
    else if (x `mod` 5 == 0) then "Buzz"
    else show x

fizzBuzz2 :: Integer -> String
fizzBuzz2 x
  | (x `mod` 3 == 0 && x `mod` 5 == 0 ) = "FizzBuzz"
  | (x `mod` 3 == 0 ) = "Fizz"
  | (x `mod` 5 == 0) = "Buzz"
  | otherwise = show x
--allTrueF :: Bool -> (Bool -> Bool)
--allTrueF x y = (x == True and y == True)

printDouble :: Int -> String
printDouble x = show ((fromIntegral x)/1)

printDouble2 :: Int -> String
printDouble2 value = show (fromIntegral value*2.0)

calEven a = if even a
              then a-1
            else  3*a+1

overwrite x = let x = 2
               in
                let x = 3
                  in
                let x = 4
                  in
                    x
--x = overwrite 5
--x = overwrite 4
--x = overwrite 5        
counter y = (\x -> x + 1)
              ((\x -> x + 1)
                ((\x -> x) y))
-- #4.2  
ifEven f x = if even x
              then f x
              else x

inc x = x + 1
ifEvenInc = (\x -> ifEven inc x)

genIfXEven f x = ifEven f x

--myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

--genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)
--exampleUrlBuilder = genHostRequestBuilder "http://example.com"
--genHostRequestBuilder host = (\apiKey resource id -> getRequestUrl host apikey resource id)

----------------
sum' :: Num a => [a] -> a --[Int] -> Int --
sum' (x:xs) = x + sum' xs
sum' []     = 0


concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' p (x:xs) = (p == x) || elem' p xs  

zip' :: [a] -> [b] -> [(a,b)]
--zip' (y:ys) (x:xs) = [(y,x)] ++ zip' ys xs
zip' (y:ys) (x:xs) = (y,x) : zip' ys xs
zip' _ _           = []

last' :: [a] -> a
last' [] = error ("There is no last item of empty list")
last' [x] = x
last' (_:xs) = last' xs

head' = undefined
-- Description:	returns the first item of a list

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 x = x
drop' i (x:xs) = if i>0  then drop' (i-1) xs else x:xs
-- Description:	returns the first item of a list

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) = if f x then  dropWhile f xs else x:xs 



init' :: [a] -> [a]
init' [] = error ("There is initiative items of empty list")
init' [x] =[]
init' (x:_) = [x]

tail':: [a] -> [a]
tail' [] = error ("There is no tail item of empty list")
tail' [x] = []
tail' (_:xs) = xs

data Rhtype = Pos | Neg
data ABOtype = A | B | AB | O

data BloodType = BloodType ABOtype Rhtype

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show,Eq,Ord)
--type Name = (String, String)--
--instance Ord Name where
    --compare (f1,l1) (f2,l2) = compare (l1,f1) (l2,f2)

data Name = Name (String, String) deriving (Show, Eq,Ord)
--instance Ord Name where
    -- compare :: Name -> Name -> Ordering
    -- compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)

names :: [Name]
names = [Name ("Emil","Cioran")
          , Name ("Eugene","Thacker")
          , Name ("Friedrich","Nietzsche")]

-- alwaysBlue ::[Color] -> Bool
-- alwaysBlue [] = False
-- alwaysBlue (x:xs) = x == Blue && alwaysBlue xs

isPrime :: Int -> Bool

isPrime n
  | n<=1 = False
  |n <=3 = True
  | otherwise = go 2
  where 
    go i 
     | i>= n = True
     | mod n i ==0 = False
     | otherwise = go (i+1)


-- -- *** INSTRUCTIONS: *** --
--   -- Open your TicHaskToe repository in Gitpod
--   -- Create a file `State.hs` in the `solutions` directory and paste the code below.
--   -- Open the `TicHaskToe.cabal` file in the root directory,
--     -- find the `executable TicHaskToe` stanza,and make the following changes:
--       -- 1. Add `, State` below `, A5` in the `other-modules:` section
--       -- 2. Add `, mtl` below `, random` in the `build-depends:` section
--   -- To test the code:
--     -- 1. Run `cabal repl` in the terminal and then `:l State`
--     -- 2. Enter `randomGame`

-- -- ignore some linter suggestions:
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use tuple-section" #-}
-- {-# HLINT ignore "Eta reduce" #-}

-- module State where

-- import A1
-- import A2
-- import A4
-- import A5

-- import System.Random (StdGen, randomR, newStdGen)
-- import Control.Monad.State

-- -- define a record type to hold the state
-- data Game = Game {
--     activePlayer :: Player
--   , status       :: GameState
--   , board        :: Board
--   , generator    :: StdGen
--   }

-- -- create an initial Game value using a random generator
-- initialState :: StdGen -> Game
-- initialState gen = Game {
--     activePlayer = X
--   , status       = Playing
--   , board        = _EMPTY_BOARD_
--   , generator    = gen
--   }

-- -- game loop
-- playGame :: State Game ()
-- playGame = do
--   Game ap _ b gen <- get
--   let opens      = getOpenMoves b
--       (i, gen')  = randomR (0, length opens - 1) gen
--       move       = opens !! i
--       (stat, b') = playMove ap b move
--   -- update the state
--   put $ Game { activePlayer = switchPlayer ap
--              , status       = stat
--              , board        = b'
--              , generator    = gen'
--              }
--   -- play again if game isn't over (otherwise return ())
--   when (stat == Playing) playGame

-- randomGame :: IO ()
-- randomGame = do
--   game <- initialState <$> newStdGen -- get a random generator and create initial game state
--   let (_, game') = runState playGame game -- call runner to get handler and apply it to initial state
--   -- display result
--   printBoard $ board game'
--   putStrLn . showGameState $ status game'

-- -- *** HELPERS TO IDENTIFY INDICES OF OPEN SQUARES *** --
-- getOpenMoves :: Board -> [Move]
-- getOpenMoves b = concat $ go 0 [] b
--   where
--     go _ ms []     = ms
--     go i ms (r:rs) = go (i + 1) (map (\j -> (i, j)) (getOpenSquares r) : ms) rs

-- getOpenSquares :: Row -> [Int]
-- getOpenSquares r = go 0 [] r
--   where
--     go _ os []       = os
--     go j os (sq:sqs)
--       | sq == E      = go (j + 1) (j:os) sqs
--       | otherwise    = go (j + 1) os     sqs