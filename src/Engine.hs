module Engine where

import qualified CodeWorld
import qualified Data.Map as Map
import Organisms (addOrganism, lifecycle)
import System.Random (Random (random), StdGen, mkStdGen, randomR)
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
                    Cell {state = Mover, coords = (0, 1)}
                  ],
                health = 10,
                direction = North,
                foodCollected = 0,
                lifetime = 0,
                mutationFactor = 1,
                lifespanFactor = 10,
                lookRange = 20,
                randomGen = mkStdGen 2
              }
          )
        ]
    grid' =
      Map.insert (0, -1) (Cell Food (0, -1)) $
        Map.insert (0, -2) (Cell Food (0, -2)) $
          Map.insert (0, -3) (Cell Food (0, -3)) $
            Map.insert (0, -4) (Cell Food (0, -4)) $
              Map.insert (0, -5) (Cell Food (0, -5)) $
                Map.insert (-1, 0) (Cell Food (-1, 0)) $
                  Map.insert (-1, -2) (Cell Food (-1, -2)) $
                    Map.fromList $
                      [ let xy = (x, y)
                         in (xy, Cell {state = Empty, coords = xy})
                        | x <- [-10 .. 10],
                          y <- [-10 .. 10]
                      ]

-- | Make default engine for given world
defaultEngineFor :: World -> Engine
defaultEngineFor w = Engine {world = w, fps = 60, gen = mkStdGen 0}

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
updateEngine _ engine
  -- TODO: match first case once per second
  | shouldUpdate = updated
  | otherwise = engine {gen = gen'}
  where
    (n, gen') = randomR (0, 100 :: Int) (gen engine)
    shouldUpdate = n > 90
    updated = engine {world = tick (world engine), gen = gen'}

-- | Start life engine
-- TODO: use activityOf (or animationOf) later
start :: IO ()
start = CodeWorld.activityOf (defaultEngineFor defaultWorld) updateEngine draw
