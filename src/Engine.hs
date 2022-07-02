module Engine where

import qualified CodeWorld
import Common
import qualified Data.Map as Map
import Organisms (addOrganism, lifecycle)
import System.Random (StdGen, mkStdGen, newStdGen, randomR)
import Types

defaultWorld :: StdGen -> World
defaultWorld gen = World grid' organisms'
  where
    organisms' =
      Map.fromList
        [ ( [(0, 0), (-1, 1), (1, -1)],
            Organism
              { anatomy =
                  [ Cell {state = Mouth, coords = (0, 0)},
                    Cell {state = Producer, coords = (-1, 1)},
                    Cell {state = Producer, coords = (1, -1)}
                  ],
                health = 10,
                direction = North,
                foodCollected = 0,
                lifetime = 0,
                mutationFactor = 1,
                lifespanFactor = 1,
                lookRange = 20,
                randomGen = gen
              }
          ),
          ( [(-8, -5), (-9, -5), (-9, -6)],
            Organism
              { anatomy =
                  [ Cell {state = Mouth, coords = (-8, -5)},
                    Cell {state = Mover, coords = (-9, -5)},
                    Cell {state = Killer, coords = (-9, -6)}
                  ],
                health = 10,
                direction = North,
                foodCollected = 0,
                lifetime = 0,
                mutationFactor = 1,
                lifespanFactor = 10,
                lookRange = 20,
                randomGen = gen
              }
          ),
          ( [(-4, -3), (-5, -3), (-4, -2)],
            Organism
              { anatomy =
                  [ Cell {state = Mouth, coords = (-4, -3)},
                    Cell {state = Mover, coords = (-5, -3)},
                    Cell {state = Mouth, coords = (-4, -2)}
                  ],
                health = 10,
                direction = South,
                foodCollected = 0,
                lifetime = 0,
                mutationFactor = 1,
                lifespanFactor = 10,
                lookRange = 20,
                randomGen = gen
              }
          ),
          ( [(4, 3), (4, 4), (3, 3), (3, 2)],
            Organism
              { anatomy =
                  [ Cell {state = Mouth, coords = (4, 3)},
                    Cell {state = Producer, coords = (4, 4)},
                    Cell {state = Producer, coords = (3, 3)},
                    Cell {state = Producer, coords = (3, 2)}
                  ],
                health = 10,
                direction = North,
                foodCollected = 0,
                lifetime = 0,
                mutationFactor = 1,
                lifespanFactor = 10,
                lookRange = 20,
                randomGen = gen
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
                        | x <- [- worldSizeX .. worldSizeX],
                          y <- [- worldSizeY .. worldSizeY]
                      ]

-- | Make default engine for given world
defaultEngineFor :: World -> Engine
defaultEngineFor w = Engine {world = w, frequency = 0.2, timeAcc = 0}

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
updateEngine (CodeWorld.TimePassing time) engine
  | shouldUpdate = updated
  | otherwise = engine {timeAcc = timeAcc engine + time}
  where
    shouldUpdate = timeAcc engine - frequency engine > 0
    updated = engine {world = tick (world engine), timeAcc = timeAcc engine - frequency engine}
updateEngine _ engine = engine

-- | Start life engine
start :: IO ()
start = do
  gen <- newStdGen
  CodeWorld.activityOf (defaultEngineFor (defaultWorld gen)) updateEngine draw
