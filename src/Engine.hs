module Engine where

import qualified CodeWorld
import Types

start :: IO ()
start = CodeWorld.drawingOf $ draw world
  where
    world = World grid'
    grid' =
      let size' = 1
       in [ Cell {state = Empty, size = size', coords = (x, y)}
            | x <- [-5 .. 5],
              y <- [-5 .. 5]
          ]

-- [ Cell {state = Empty, coords = (0, 0), size = size'},
--   Cell {state = Empty, coords = (1, 0), size = size'},
--   Cell {state = Empty, coords = (1, 1), size = size'}
-- ]