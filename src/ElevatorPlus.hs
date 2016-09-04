module ElevatorPlus
    (
    ) where

class Elevator e where
  floor :: e -> Integer
  identity :: e -> Integer
  goalFloor :: e -> Integer
  stepE :: e -> e
  isIdle :: e -> Bool
  sameId :: e -> e -> Bool

class PickupRequest p where
  floor :: p -> Integer
  direction :: p -> Bool -- True for up, False for down

class ElevatorSystem es where
  status :: Elevator e => es -> [e]
  update :: Elevator e => es -> e -> es
  step :: es -> es
  pickup :: PickupRequest p => es -> p -> es
