module Main where

import Lib

main :: IO ()
main = print (hanoi 5 "a" "b" "c")

  {-putStrLn "input?"
  >> getLine
  >>= \input -> print (someFunc(read input))-}
