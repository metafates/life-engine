module Organisms where

import Data.Bifunctor (Bifunctor (first, second))
import Data.Function (on)
import Data.List (find, sortBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, mapMaybe)
import System.Random (StdGen, randomR)
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

tryUpdateOrganismWith ::
  (Organism -> Organism) -> (Organism, World) -> (Organism, World)
tryUpdateOrganismWith update (organism, world) =
  if isValidPosition updatedCoords
    then (updatedOrganism, updatedWorld)
    else (organism, world)
  where
    updatedWorld =
      let organisms' =
            Map.insert updatedCoords updatedOrganism $
              Map.delete (organismBodyCoords organism) (organisms world)
          grid' =
            foldl (flip (Map.adjust toEmpty)) (grid world) updatedCoords
          toEmpty c = c {state = Empty}
       in world {organisms = organisms', grid = grid'}
    updatedOrganism = update organism
    updatedCoords = organismBodyCoords updatedOrganism
    -- checks if cells at given coordinates are empty and are in bounds of grid
    isValidPosition =
      all ((\c -> isJust c && (state (fromJust c) == Empty)) . (`cellAt` world))

-- | Try to move at next direction
tryMove :: (Organism, World) -> (Organism, World)
tryMove = tryUpdateOrganismWith move
  where
    move organism =
      let anatomy' = anatomy organism
          dir = direction organism
          shifted c = c {coords = shiftTo dir (coords c)}
       in randomlyDirected
            organism
              { anatomy = map shifted anatomy'
              }

-- | Try to rotate at next direction
tryRotate :: (Organism, World) -> (Organism, World)
tryRotate = tryUpdateOrganismWith rotate
  where
    rotate organism =
      let anatomy' = anatomy organism
          rotationRule (x, y) = (- y, x)
          rotated c = c {coords = rotationRule (coords c)}
       in organism
            { anatomy = map rotated anatomy'
            }

-- | Sets organism direction to the next random one
randomlyDirected :: Organism -> Organism
randomlyDirected organism =
  if d >= 5
    then organism {direction = nextDirection, randomGen = gen'}
    else organism {randomGen = gen}
  where
    directions =
      let dir = direction organism
       in filter (dir /=) [North, East, South, West]
    (nextDirection, gen) = randomChoice (randomGen organism) directions
    (d, gen') = randomR (0, 10 :: Int) gen

addCellsAt :: [Coords] -> CellState -> World -> World
addCellsAt cs state w =
  foldl
    (\w c -> w {grid = Map.insert c (Cell state c) (grid w)})
    w
    cs

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
      let coords = organismBodyCoords organism
       in addCellsAt coords Food (world {organisms = Map.delete coords (organisms world)})

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
          with = flip Map.union (grid world)
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
      let neighbors = zipWith vectorSum adjacent . repeat
       in mapMaybe (`cellAt` world) . neighbors . coords

    handleFood food =
      let foodCollected' = foodCollected organism
          foodCoords = map coords food
          updatedGrid =
            Map.fromList $
              filter ((`notElem` foodCoords) . fst) $
                Map.toList (grid world)
       in ( organism {foodCollected = foodCollected' + length food},
            addCellsAt (map coords food) Empty world {grid = updatedGrid}
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
addOrganism organism world
  | isValidOrganismPosition organism world = world {organisms = organisms'}
  | otherwise = world
  where
    organisms' =
      let key = organismBodyCoords organism
       in Map.insert key organism (organisms world)

isValidOrganismPosition :: Organism -> World -> Bool
isValidOrganismPosition organism world =
  all (`isFreeAt` world) (organismBodyCoords organism)

-- | Mutates organism
-- TODO: make mutation happen and not just copy it
mutate :: Organism -> (Organism, StdGen)
mutate organism = (organism {foodCollected = 0, lifetime = 0}, randomGen organism)

-- | Try to reproduce
-- This also returns a created organism
-- TODO
tryReproduce :: (Organism, World) -> (Organism, World)
tryReproduce (organism, world)
  | canReproduce = reproduced
  | otherwise = (organism, world)
  where
    -- Once an organism eats more food than amount of its body cells it will reproduce.
    canReproduce =
      foodCollected organism >= length (anatomy organism)

    reproduced =
      let (offspring, gen) = mutate organism

          -- set offspring coordinates to the next available one
          -- todo: try different options and find the best one,
          offspring' =
            let variants =
                  [ second (subtract 4),
                    bimap (+ 4),
                    second (+ 4),
                    second (+ 4),
                    first (subtract 4),
                    bimap (subtract 4),
                    first (+ 4)
                  ]
             in map (\f -> offspring {anatomy = map (\c -> c {coords = f (coords c)}) (anatomy offspring)}) variants

          updatedWorld = case find (`isValidOrganismPosition` world) offspring' of
            Nothing -> world
            Just o -> addOrganism o world
       in -- updatedWorld = addOrganism offspring' world
          (organism {foodCollected = 0, randomGen = gen}, updatedWorld)

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
lifecycle :: (Organism, World) -> (Maybe Organism, World)
lifecycle (organism, world)
  | hasMover organism = moverLifecycle
  | otherwise = producerLifecycle
  where
    -- Movers do not make food, but they can move
    moverLifecycle =
      let moverSequence = (tryMove . tryRotate . tryReproduce . tryEatFood)
       in case tryDie (organism, world) of
            (Nothing, w) -> (Nothing, w)
            (Just o, w) -> first Just $ moverSequence (o, w)

    -- Producers do not move, but they can make food
    producerLifecycle =
      let producerSequence = (tryReproduce . tryEatFood . tryMakeFood)
       in case tryDie (organism, world) of
            (Nothing, w) -> (Nothing, w)
            (Just o, w) -> first Just $ producerSequence (o, w)
