module Elevator where

data Elevator = Elev {eId::Integer, floorNumber::Integer, goalFloorNumber::Integer} deriving Show
-- direction: negative for down, positive for up
data PickupRequest = PR {floor::Integer, direction::Integer} deriving Show
type ElevatorSystem = [Elevator]

status :: ElevatorSystem -> [Elevator]
status = id

sameId :: Elevator -> Elevator -> Bool
sameId a b = (eId a) == (eId b)

-- update an elevator status
update :: ElevatorSystem -> Elevator -> ElevatorSystem
update [] ne = [ne]
update (e:es) ne | sameId e ne = ne:es
                 | otherwise = e:(update es ne)

stepE :: Elevator -> Elevator
stepE (Elev i f g) | f == g = Elev i f g
                | f < g = Elev i (f + 1) g
                | f > g = Elev i (f - 1) g

step :: ElevatorSystem -> ElevatorSystem
step = fmap stepE

isIdle :: Elevator -> Bool
isIdle (Elev i f g) = f == g

receiveRequest :: Elevator -> PickupRequest -> Elevator
receiveRequest (Elev i f g) (PR rf _) = Elev i f rf

-- effort :: PickupRequest -> Elevator -> Integer
-- effort  e | isIdle e = Tru
-- effort (PR f d) (Elev _ ef g) | ef == g = abs $ f - ef

pickup :: ElevatorSystem -> PickupRequest -> ElevatorSystem
pickup [] _ = []
pickup (e:es) pr | isIdle e = receiveRequest e pr : es
                 | otherwise = e:(pickup es pr)
