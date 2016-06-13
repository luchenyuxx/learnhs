{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List

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
  deriving Show

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = attackerRoll a >>= \ar -> defenderRoll d >>=
          \br -> let r = calculateRollResults ar br in
          return (Battlefield (a + fst r) (d + snd r))

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d) | a < 2 || d <= 0 = return b
                           | otherwise = battle b >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb b = allCases b >>= \bs ->
                return $ fromIntegral (foldr (\a b -> b + (attackerWin a)) 0 bs) / 1000

allCases :: Battlefield -> Rand StdGen [Battlefield]
allCases = replicateM 1000 . invade

attackerWin :: Battlefield -> Int
attackerWin (Battlefield a d) | a > 0 && d == 0 = 1
                              | otherwise = 0

rollN :: Int -> Rand StdGen [DieValue]
rollN n = replicateM n die

attackerN :: Army -> Army
attackerN n | n > 3 = 3
            | n > 1 = n
            | otherwise = 0

defenderN :: Army -> Army
defenderN n | n > 2 = 2
            | n > 0 = n
            | otherwise = 0

attackerRoll :: Army -> Rand StdGen [DieValue]
attackerRoll = rollN . attackerN

defenderRoll :: Army -> Rand StdGen [DieValue]
defenderRoll = rollN . defenderN

calculateRollResults :: [DieValue] -> [DieValue] -> (Army, Army)
calculateRollResults a d = foldr1 plusP $ map comparePair $ zip (sortD a) (sortD d)

comparePair :: (DieValue, DieValue) -> (Army, Army)
comparePair (DV a, DV d) | a > d = (0, -1)
                   | otherwise = (-1, 0)

plusP :: (Army, Army) -> (Army, Army) -> (Army, Army)
plusP (a1, d1) (a2, d2) = (a1 + a2, d1 + d2)

sortD :: Ord a => [a] -> [a]
sortD = reverse . sort

testBattlefield :: Battlefield
testBattlefield = Battlefield 5 10
