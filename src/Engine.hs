module Engine where

import qualified CodeWorld
import Types

start :: IO ()
start = CodeWorld.drawingOf $ draw world
  where
    world = World grid' 60
    grid' =
      let size' = 1
       in [ Cell {state = Empty, size = size', coords = (x, y)}
            | x <- [-5 .. 5],
              y <- [-5 .. 5]
          ]