module Organisms where

import Data.Bifunctor (Bifunctor (first, second))
import Data.Function (on)
import Data.List (find, sortBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, mapMaybe)
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

hasProducer :: Organism -> Bool
hasProducer = hasCellOfState Producer

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

-- | Shifts coordinates to the direction by 1
shiftTo :: Direction -> Coords -> Coords
shiftTo North = second (+ 1)
shiftTo East = first (+ 1)
shiftTo South = second (subtract 1)
shiftTo West = first (subtract 1)

-- | Try to move at next direction
tryMove :: (Organism, World) -> (Organism, World)
tryMove (organism, world) =
  if isValidPosition movedOrganismCoords
    then (movedOrganism, worldWithMoved)
    else (organism, world)
  where
    worldWithMoved =
      let organisms' =
            Map.insert movedOrganismCoords movedOrganism $
              Map.delete (organismBodyCoords organism) (organisms world)
          grid' =
            foldl (flip (Map.adjust toEmpty)) (grid world) movedOrganismCoords
          toEmpty c = c {state = Empty}
       in world {organisms = organisms', grid = grid'}
    -- organism moved to next direction
    movedOrganism =
      let anatomy' = anatomy organism
          dir = direction organism
          shifted c = c {coords = shiftTo dir (coords c)}
       in organism
            { anatomy = map shifted anatomy',
              direction = nextDirection dir
            }
    movedOrganismCoords = organismBodyCoords movedOrganism
    -- checks if cells at given coordinates are empty and are in bounds of grid
    isValidPosition =
      all ((\c -> isJust c && (state (fromJust c) == Empty)) . (`cellAt` world))

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
tryDie :: (Organism, World) -> (Maybe Organism, World)
tryDie (organism, world)
  | shouldDie = (Nothing, worldWithoutOrganism)
  | otherwise = (Just (organism {lifetime = lifetime' + 1}), world)
  where
    lifetime' = lifetime organism
    -- Organism can either die from having 0 hp or from getting too old
    shouldDie =
      health organism == 0
        || lifetime' >= length (anatomy organism) * lifespanFactor organism
    worldWithoutOrganism =
      world
        { organisms = Map.delete (organismBodyCoords organism) (organisms world)
        }

-- | Activates producer cell
tryMakeFood :: (Organism, World) -> (Organism, World)
tryMakeFood (organism, world)
  | hasMover organism || (not . hasProducer) organism = (organism, world)
  | otherwise =
    let producers = filter ((==) Producer . state) (anatomy organism)
     in (organism, addToGrid $ concatMap makeAdjacentFood producers)
  where
    makeAdjacentFood = map (Cell Food) . relativeTo adjacent . coords
    addToGrid =
      let foodGrid =
            foldl
              ( \m f ->
                  let c = coords f
                   in if isFreeAt c world then Map.insert c f m else m
              )
              Map.empty
          with = Map.union (grid world)
          updateGrid g = world {grid = g}
       in updateGrid . with . foodGrid

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
organismBodyCoords :: Organism -> [Coords]
organismBodyCoords = sortBy (compare `on` integralCantor) . map coords . anatomy

-- | Adds organism to the world
addOrganism :: Organism -> World -> World
addOrganism organism world = world {organisms = organisms'}
  where
    organisms' =
      let key = organismBodyCoords organism
       in Map.insert key organism (organisms world)

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
