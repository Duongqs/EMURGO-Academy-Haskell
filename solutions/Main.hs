module Main where
import A1
import A2
import A3
import A4
import A5
-- import State
main :: IO ()
main = do
   firstPlayer _RANDOM_BOOL_ >>= play _EMPTY_BOARD_
