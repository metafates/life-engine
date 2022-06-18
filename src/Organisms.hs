module Organisms where

import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
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
hasBrain organism = hasMover organism && hasEye organism

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
tryDie = undefined

-- | Activates producer cell
-- TODO: spawn food cells in adjacent coordinates
tryMakeFood :: (Organism, World) -> (Organism, World)
tryMakeFood = undefined

-- | Eat food if there are any nearby
tryEatFood :: (Organism, World) -> (Organism, World)
tryEatFood (organism, world) =
  case find ((==) Mouth . state) (anatomy organism) of
    Nothing -> error "Organism must have a mouth"
    Just mouth -> handleFood $ filter ((==) Food . state) (cellsAround mouth)
  where
    cellsAround =
      let neighbors = zipWith vectorSum around . repeat
       in mapMaybe (`cellAt` world) . neighbors . coords

    handleFood food =
      let foodCollected' = foodCollected organism
          foodCoords = map coords food
          updatedGrid =
            Map.fromList $
              filter ((`notElem` foodCoords) . fst) $
                Map.toList (grid world)
       in ( organism {foodCollected = foodCollected' + length food},
            world {grid = updatedGrid}
          )

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

-- | Returns cell at coordinates.
-- if coordinates are out of bounds nothing is returned
cellAt :: Coords -> World -> Maybe Cell
cellAt xy world =
  case Map.lookup xy (grid world) of
    Just cell -> Just cell
    Nothing -> case find (elem xy . fst) (Map.toList (organisms world)) of
      Nothing -> Nothing
      Just (_, organism) -> find ((==) xy . coords) (anatomy organism)

-- | Checks if cell is free (empty) at coordinates
isFreeAt :: Coords -> World -> Bool
isFreeAt coords world = toBool $ cellAt coords world
  where
    toBool Nothing = False
    toBool (Just cell) = state cell == Empty

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
