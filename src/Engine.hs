module Engine where

import qualified CodeWorld
import qualified Data.Map as Map
import Organisms (addOrganism, lifecycle)
import System.Random (mkStdGen)
import Types

defaultWorld :: World
defaultWorld = World grid' organisms'
  where
    organisms' =
      Map.fromList
        [ ( [(0, 0), (0, 1)],
            Organism
              { anatomy =
                  [ Cell {state = Mouth, coords = (0, 0)},
                    Cell {state = Producer, coords = (0, 1)}
                  ],
                health = 10,
                direction = North,
                foodCollected = 0,
                lifetime = 0,
                mutationFactor = 1,
                lifespanFactor = 1,
                lookRange = 20,
                randomGen = mkStdGen 2
              }
          )
        ]
    grid' =
      Map.insert (0, -1) (Cell Food (0, -1)) $
        Map.insert (-1, 0) (Cell Food (-1, 0)) $
          Map.insert (-1, -1) (Cell Food (-1, -1)) $
            Map.fromList $
              [ let xy = (x, y)
                 in (xy, Cell {state = Empty, coords = xy})
                | x <- [-10 .. 10],
                  y <- [-10 .. 10]
              ]

-- | Make default engine for given world
defaultEngineFor :: World -> Engine
defaultEngineFor w = Engine {world = w, fps = 60}

-- | Applies lifecycle for each organism
applyLifecycle :: Map.Map [Coords] Organism -> World -> World
applyLifecycle toVisit world
  | Map.null toVisit = world
  | otherwise =
    case organism' of
      Nothing -> applyLifecycle toVisit' world'
      Just o -> applyLifecycle toVisit' $ addOrganism o world'
  where
    (coords, organism) = head $ Map.toList toVisit
    toVisit' = Map.delete coords toVisit
    (organism', world') = lifecycle (organism, world)

-- | Update world state
tick :: World -> World
tick world = applyLifecycle (organisms world) world

-- | Update engine from given event
updateEngine :: CodeWorld.Event -> Engine -> Engine
updateEngine (CodeWorld.TimePassing seconds) engine
  -- TODO: match first case once per second
  | True = updated
  | otherwise = engine
  where
    updated = engine {world = tick (world engine)}
updateEngine _ engine = engine

-- | Start life engine
-- TODO: use activityOf (or animationOf) later
start :: IO ()
start = CodeWorld.activityOf (defaultEngineFor defaultWorld) updateEngine draw
