{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield

rollN :: Int -> Rand StdGen [DieValue]
roll n = sequence $ take n $ repeat die

attackerN :: Army -> Army
attackerN n | n > 3 = 3
            | n >= 1 = n - 1
            | otherwise = 0

defenderN :: Army -> Army
defenderN n | n >= 2 = 2
            | n >= 0 = n
            | otherwise = 0
