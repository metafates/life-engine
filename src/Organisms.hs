module Organisms where

import Data.List (find)
import qualified Data.Map as Map
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
-- TODO: move organism to the next direction
attemptMove :: (Organism, World) -> (Organism, World)
attemptMove = undefined

-- | Try to rotate at next direction
-- TODO: rotate organism at the direction and update direction
attemptRotate :: (Organism, World) -> (Organism, World)
attemptRotate = undefined

-- | Get next direction
-- North -> East -> South -> West -> repeat
-- TODO
nextDirection :: Direction -> Direction
nextDirection = undefined

-- | Kill an organism
-- TODO: remove organism from the world
die :: (Organism, World) -> World
die = undefined

-- | Activates producer cell
-- TODO: spawn food cells in adjacent coordinates
makeFood :: (Organism, World) -> (Organism, World)
makeFood = undefined

-- | Gets organism at given coordinates of 1 cell
organismAtCoords :: Coords -> World -> Maybe Organism
organismAtCoords coords world =
  case find (elem coords) $ Map.keys $ organisms world of
    Nothing -> Nothing
    Just key -> Map.lookup key (organisms world)

-- | Gets coordinates of organism cells
-- Returned list of coordinates is *always* in the same order
-- TODO: use pairing function to sort coordinates by
-- HINT: Use `cantor` from Utils.hs
organismBodyCoords :: Organism -> [Coords]
organismBodyCoords = undefined

-- | Adds organism to the world
-- TODO: use organism body size as key and organism as value
addOrganism :: Organism -> World -> World
addOrganism organism world = world {organisms = organisms'}
  where
    organisms' = undefined
