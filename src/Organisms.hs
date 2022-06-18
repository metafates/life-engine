module Organisms where

import Data.List (find)
import qualified Data.Map as Map
import Types

import Utilities

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
hasEye :: Organism -> Bool
hasEye = hasCellOfState Eye

-- | Checks if organism has brain
-- Organism gets brain when it has both eyes and mover cell
hasBrain :: Organism -> Bool
hasBrain organism = hasMover organism && hasEyes organism

-- | Amount of food required before it can reproduce
foodNeeded :: Organism -> Int
foodNeeded = length . anatomy

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
nextDirection North = East
nextDirection East = South
nextDirection South = West
nextDirection West = North

-- | Kill organism if it has 0 hp
-- TODO
tryDie :: (Organism, World) -> (Maybe Organism, World)
tryDie (org, world) = 
  | health org == 0 = (updWorld)
  | otherwise = (org, world)
  where
    updWorld = map Map.delete (coords cell) (grid world)

-- | Activates producer cell
-- TODO: spawn food cells in adjacent coordinates
tryMakeFood :: (Organism, World) -> (Organism, World)
tryMakeFood (org, world) = 
  case find ((==) Mouth) (anatomy org) of
      Nothing -> (org, world)
      Just month ->

-- | Eat food if there are any nearby
-- TODO
tryEatFood :: (Organism, World) -> (Organism, World)
tryEatFood (org, world) = 
  case find ((==) Mouth) (anatomy org) of
      Nothing -> (org, world)
      Just month ->
        case find ((==) Food) possibleCells of
          Nothing -> (org, world)
          Just food -> (updOrg, updWorld)
        where
          possibleCells = zipWith vectorSum coords month adjacent

          updWorld = Map.delete (coords food) (grid world)
          updOrg = org{foodCollected}


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
-- This also returns a created organism
-- TODO
tryReproduce :: (Organism, World) -> ((Organism, World), Maybe Organism)
tryReproduce (organism, world)
  | canReproduce = reproduce (organism, world)
  | otherwise = ((organism, world), Nothing)
  where
    canReproduce = undefined
    reproduce = undefined

isFreeAt :: Coords -> World -> Bool
isFreeAt coords world = all not [overlapWithOrganism, overlapWithGridCell]
  where
    overlapWithOrganism = elem coords $ concat $ Map.keys (organisms world)
    overlapWithGridCell = Map.member coords (grid world)

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
