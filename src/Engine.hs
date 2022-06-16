module Engine where

import qualified CodeWorld
import qualified Data.Map as Map
import Types

defaultWorld :: World
defaultWorld = World grid' organisms'
  where
    organisms' = Map.empty -- TODO: add 1 sample organism to test
    grid' =
      Map.fromList $
        [ let xy = (x, y)
           in (xy, Cell {state = Empty, size = 1, coords = xy})
          | x <- [-5 .. 5],
            y <- [-5 .. 5]
        ]

-- | Make default engine for given world
defaultEngineFor :: World -> Engine
defaultEngineFor w = Engine {world = w, fps = 60}

-- | Update world state
-- TODO
tick :: World -> World
tick = undefined
  where
    go world = lifecycle (or)

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
