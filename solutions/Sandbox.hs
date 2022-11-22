module Sandbox where
import Text.Read (Lexeme(String, Char))
import Foreign (toBool)
half :: Int -> Double
half n = (fromIntegral n)/2.0

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
