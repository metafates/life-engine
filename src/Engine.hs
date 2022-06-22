module Engine where

import qualified CodeWorld
import qualified Data.Map as Map
import Organisms (addOrganism, lifecycle)
import Types

defaultWorld :: World
defaultWorld = World grid' organisms'
  where
    organisms' = Map.empty -- TODO: add 1 sample organism to test
    grid' =
      Map.fromList $
        [ let xy = (x, y)
           in (xy, Cell {state = Empty, coords = xy})
          | x <- [-5 .. 5],
            y <- [-5 .. 5]
        ]

-- | Make default engine for given world
defaultEngineFor :: World -> Engine
defaultEngineFor w = Engine {world = w, fps = 60}

-- | Applies lifecycle for each organism
applyLifecycle :: Map.Map [Coords] Organism -> World -> World
applyLifecycle toVisit world
  | Map.null toVisit = world
  | otherwise = case organism' of
    Nothing -> applyLifecycle toVisit' world'
    Just o -> applyLifecycle toVisit' $ addOrganism o world'
  where
    (coords, organism) = head $ Map.toList toVisit
    toVisit' = Map.filterWithKey (\k _ -> k /= coords) toVisit
    (organism', world') = lifecycle (organism, world)

-- | Update world state
tick :: World -> World
tick world = applyLifecycle (organisms world) world

-- | Update engine from given event
updateEngine :: CodeWorld.Event -> Engine -> Engine
updateEngine (CodeWorld.TimePassing seconds) engine
  -- I hope this fps lock formula works...
  | round (seconds * 1000) `rem` fps engine == 0 = updated
  | otherwise = engine
  where
    updated = engine {world = tick (world engine)}
updateEngine _ engine = engine

-- | Start life engine
-- TODO: use activityOf (or animationOf) later
start :: IO ()
start = CodeWorld.drawingOf (draw defaultWorld)
