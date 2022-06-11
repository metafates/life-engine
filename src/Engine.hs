module Engine where

import qualified CodeWorld
import qualified Data.Map as Map
import Types

updateWorld :: CodeWorld.Event -> World -> World
updateWorld = undefined

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

defaultEngine :: Engine
defaultEngine = Engine {world = defaultWorld, fps = 60}

start :: IO ()
start = CodeWorld.drawingOf $ draw $ world defaultEngine