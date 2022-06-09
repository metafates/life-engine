module Types where

import qualified CodeWorld
import Data.Map (Map)
import qualified Data.Map as Map
import Utilities (bimap)

-- | (X, Y) coordinates
type Coords = (Int, Int)

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq)

-- | State which cell could take
data CellState
  = Mouth
  | Producer
  | Mover
  | Killer
  | Armor
  | Eye Direction
  | Food
  | Empty
  | Wall
  deriving (Eq)

-- | Cell
data Cell = Cell
  { state :: CellState,
    size :: Double
  }

-- | Organism
data Organism = Organism
  { anatomy :: [Cell],
    health :: Int,
    direction :: Maybe Direction,
    foodEaten :: Int,
    mutationFactor :: Double,
    lifespanFactor :: Double,
    lookRange :: Int
  }

-- | World
data World = World
  { grid :: Map Coords Cell,
    fps :: Int
  }

class Drawable a where
  draw :: a -> CodeWorld.Picture

instance Drawable Cell where
  draw cell = figure
    where
      size' = size cell
      square = CodeWorld.solidRectangle size' size'
      -- TODO: draw it in different colors
      figure = case state cell of
        Mouth -> square
        Producer -> square
        Mover -> square
        Killer -> square
        Armor -> square
        Eye direction -> square -- TODO: draw eye with direction
        Food -> square
        Empty -> square
        Wall -> square

instance Drawable World where
  draw = CodeWorld.pictures . map drawAtCoords . Map.toList . grid
    where
      drawAtCoords (coords, cell) =
        let (x', y') = bimap ((*) (size cell) . fromIntegral) coords
         in CodeWorld.translated x' y' (draw cell)
