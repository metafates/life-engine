module Organisms where

import Data.List (find)
import Types

-- | Checks if organism has a specific cell state in its anatomy
hasCellOfState :: CellState -> Organism -> Bool
hasCellOfState cell = asBool . find ((==) cell . state) . anatomy
  where
    asBool Nothing = False
    asBool (Just _) = True

-- | Checks if organism has mover cell
hasMover :: Organism -> Bool
hasMover = hasCellOfState Mover

-- | Checks if organism has eyes
-- TODO
hasEyes :: Organism -> Bool
hasEyes = undefined

-- | Checks if organism has brain
-- Organism gets brain when it has both eyes and mover cell
-- TODO
hasBrain :: Organism -> Bool
hasBrain = undefined

-- | Amount of food required before it can reproduce
-- TODO
foodNeeded :: Organism -> Int
foodNeeded = undefined

-- | Try to move at next direction
-- TODO
attemptMove :: (Organism, World) -> (Organism, World)
attemptMove = undefined

-- | Try to rotate at next direction
-- TODO
attemptRotate :: (Organism, World) -> (Organism, World)
attemptRotate = undefined

-- | Kill an organism
-- TODO
die :: (Organism, World) -> World
die = undefined

makeFood :: (Organism, World) -> (Organism, World)
makeFood = undefined