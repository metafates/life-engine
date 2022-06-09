module Engine where

import qualified CodeWorld
import qualified Data.Map as Map
import Types

defaultWorld :: World
defaultWorld = World grid' 60
  where
    grid' = Map.fromList $ zip coords (repeat cell)
    cell = Cell {state = Empty, size = 1}
    coords = [(x, y) | x <- [-5 .. 5], y <- [-5 .. 5]]

start :: IO ()
start = CodeWorld.drawingOf (draw defaultWorld)