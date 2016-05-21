module Lib
    ( someFunc,
    hanoi
    ) where

someFunc :: Integer -> Integer
someFunc n = sumDigits(doubleEveryOther (toDigits n))

-- credit card varification

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOtherRev :: [Integer] -> [Integer]
sumDigits :: [Integer] -> Integer
sumDigit :: Integer -> Integer
validate :: Integer -> Bool

toDigitsRev n
  | n < 0 = []
  | n `div` 10 == 0 = [n]
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits n = reverse (toDigitsRev n)

doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:(y:zs)) = x : (2 * y) : doubleEveryOther zs

doubleEveryOther n = reverse (doubleEveryOtherRev(reverse n))

sumDigit n = sum(toDigits n)

sumDigits [] = 0
sumDigits (x:[]) = sumDigit x
sumDigits (x:xs) = (sumDigit x) + (sumDigits xs)

validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

--tower of hanoi

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi 0 a b c = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ (hanoi (n-1) c b a)

--Log

--parseMessage :: String -> LogMessage
