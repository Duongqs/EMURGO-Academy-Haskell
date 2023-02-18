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

