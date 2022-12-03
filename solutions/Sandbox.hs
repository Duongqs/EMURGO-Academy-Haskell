module Sandbox where
import Text.Read (Lexeme(String, Char))
import Foreign (toBool)
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

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)
exampleUrlBuilder = genHostRequestBuilder "http://example.com"
genHostRequestBuilder host = (\apiKey resource id -> getRequestUrl host apikey resource id)

