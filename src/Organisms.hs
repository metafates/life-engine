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
hasEyes = hasCellOfState Eye

-- | Checks if organism has brain
-- Organism gets brain when it has both eyes and mover cell
-- TODO
hasBrain :: Organism -> Bool
hasBrain = hasMover and hasEyes

-- | Count number of Cells
countCells :: [Cell] -> Int
countCells [] = 0
countCells (c:cs) = 1 + countCells cs

-- | Amount of food required before it can reproduce
foodNeeded :: Organism -> Int
foodNeeded org = countCells org.anatomy

-- | Try to move at next direction
-- TODO: move organism to the next direction
tryMove :: (Organism, World) -> (Organism, World)
tryMove = undefined

-- | Try to rotate at next direction
-- TODO: rotate organism at the direction and update direction
tryRotate :: (Organism, World) -> (Organism, World)
tryRotate = undefined

-- | Get next direction
-- North -> East -> South -> West -> repeat
nextDirection :: Direction -> Direction
nextDirection dir
  | dir == North = East
  | dir == East = South
  | dir == South = West
  | dir == West = North

-- | Kill organism if it has 0 hp
-- TODO: remove organism from the world
tryDie :: (Organism, World) -> (Maybe Organism, World)
tryDie = undefined

-- | Activates producer cell
-- TODO: spawn food cells in adjacent coordinates
tryMakeFood :: (Organism, World) -> (Organism, World)
tryMakeFood = undefined

-- | Eat food if there are any nearby
-- TODO
tryEatFood :: (Organism, World) -> (Organism, World)
tryEatFood = undefined

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

-- | Try to reproduce
-- TODO
tryReproduce :: (Organism, World) -> (Organism, World)
tryReproduce = undefined

-- | Organism lifecycle
-- TODO
lifecycle :: (Organism, World) -> (Maybe Organism, World)
lifecycle (organism, world)
  | hasMover organism = moverLifecycle
  | otherwise = producerLifecycle
  where
    -- Movers do not make food, but they can move
    moverLifecycle =
      case tryDie (organism, world) of
        (Nothing, w) -> (Nothing, w)
        (Just organism', world') -> undefined

    -- Producers do not move, but they can make food
    producerLifecycle =
      case tryDie (organism, world) of
        (Nothing, w) -> (Nothing, w)
        (Just organism', world') -> undefined
