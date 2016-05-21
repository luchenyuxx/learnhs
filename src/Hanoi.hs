module Hanoi
    (
    honoi
    ) where

--tower of hanoi

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi 0 a b c = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ (hanoi (n-1) c b a)
