{- Problem: You have 100 doors in a row that are all initially closed. 
You make 100 passes by the doors. The first time through, you visit every door 
and toggle the door (if the door is closed, you open it; if it is open, you close it). 
The second time you only visit every 2nd door (door #2, #4, #6, ...). The third time, 
every 3rd door (door #3, #6, #9, ...), etc, until you only visit the 100th door.

Question: What state are the doors in after the last pass? Which are open, which are closed?
-}

import Data.Vect

-- Creates list from 0 to n (not including n) 
upTo : (m : Nat) -> Vect m (Fin m)
upTo Z = []
upTo (S n) = 0 :: (map FS (upTo n))

data DoorState = DoorOpen | DoorClosed

toggleDoor : DoorState -> DoorState
toggleDoor DoorOpen = DoorClosed
toggleDoor DoorClosed = DoorOpen

isOpen : DoorState -> Bool
isOpen DoorOpen = True
isOpen DoorClosed = False

initialDoors : Vect 100 DoorState
initialDoors = fromList $ map (\_ => DoorClosed) [1..100]

iterate : (n : Fin m) -> Vect m DoorState -> Vect m DoorState
iterate n doors {m} = 
  map (\(idx, doorState) => 
          if ((S (finToNat idx)) `mod` (S (finToNat n))) == Z 
              then toggleDoor doorState 
              else doorState)  
      (zip (upTo m) doors)

-- Returns all doors left open at the end
solveDoors : List (Fin 100)
solveDoors = 
  findIndices isOpen $ foldl (\doors,val => iterate val doors) initialDoors (upTo 100)

main : IO ()
main = print $ map (\n => S (finToNat n)) solveDoors
