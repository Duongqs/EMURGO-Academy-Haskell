--fizzBuzz :: Int -> String 
--fizzBuzz x =
--    if (x `mod` 3 ==0 && x `mod` 5 ==0) then "FizzBuzz"
--    else if (x `mod` 3 ==0 ) then "Fizz"
--    else if (x `mod` 5 ==0) then "Buzz"
--    else show x
power :: Integer -> Integer 
power x | x == 0 = 1 -- 1st guard
       | x /= 0 = x * x -- 2nd guard
main = do 
   putStrLn "The square of 10 is:"  -- Adding text for the output
   print (power 10) -- printing to screen    