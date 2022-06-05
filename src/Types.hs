module Types where

import qualified CodeWorld
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
    coords :: Coords,
    size :: Double
  }

-- | Organism
data Organism = Organism
  { anatomy :: [Cell],
    health :: Int,
    direction :: Maybe Direction,
    foodEaten :: Int,
    mutationFactor :: Double,
    lifespanFactor :: Double
  }

-- | World
newtype World = World {grid :: [Cell]}

class Drawable a where
  draw :: a -> CodeWorld.Picture

instance Drawable Cell where
  draw cell = positioned figure
    where
      size' = size cell
      square = CodeWorld.solidRectangle size' size'
      positioned =
        let (x, y) = bimap ((* size') . fromIntegral) $ coords cell
         in CodeWorld.translated x y
      figure = case state cell of
        Mouth -> square
        Producer -> square
        Mover -> square
        Killer -> square
        Armor -> square
        Eye _ -> square
        Food -> square
        Empty -> square
        Wall -> square

instance Drawable World where
  draw = CodeWorld.pictures . map draw . grid
