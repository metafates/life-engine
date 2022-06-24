module Types where

import qualified CodeWorld
import Common (cellSize)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (StdGen)
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
  | Eye
  | Food
  | Empty
  | Wall
  deriving (Eq)

-- | Cell
data Cell = Cell
  { state :: CellState,
    coords :: Coords
  }

-- | Organism
-- | The organism consists of cells (anatomy)
-- | Health parameter defines amount of an organism's health
-- | Direction parameter defines where the organism move to at a given frame (west, east, north, south)
-- | foodCollected is a parameter that shows amount of food eaten by an organism. 
-- | (once an organism eats more food than amount of its body cells it will reproduce)
-- | lifetime parameter shows how many frames a given organism has lived.
-- | The length of an organism's life is equal to number of its cells multiplied by lifespan multiplier 
-- | (100 by default but this number can alter because of mutations).
-- | The eye looks forward and and sees the first cells within a certain range (lookRange).
-- | randomGen is used for creating random direction for organisms (with a mover cell).
data Organism = Organism
  { anatomy :: [Cell],
    health :: Int,
    direction :: Direction,
    foodCollected :: Int,
    lifetime :: Int,
    mutationFactor :: Double,
    lifespanFactor :: Int,
    lookRange :: Int,
    randomGen :: StdGen
  }

-- | World
data World = World
  { -- | Stores only non organism cells (Walls, food & empty cells)
    grid :: Map Coords Cell,
    -- | Organisms
    organisms :: Map [Coords] Organism
  }

data Engine = Engine
  { world :: World,
    fps :: Int
  }

class Drawable a where
  draw :: a -> CodeWorld.Picture

instance Drawable Cell where
  draw cell = positioned figure
    where
      positioned =
        let (x', y') = bimap ((*) cellSize . fromIntegral) (coords cell)
         in CodeWorld.translated x' y'
      figure =
        let square = CodeWorld.solidRectangle cellSize cellSize
            colored = CodeWorld.colored
            rgb = CodeWorld.RGB
         in case state cell of
              -- pink mouth
              Mouth -> colored CodeWorld.pink square
              -- green producer
              Producer -> colored CodeWorld.green square
              -- blue mover
              Mover -> colored (rgb 0.2 0.2 0.8) square
              -- red killer
              Killer -> colored (rgb 0.8 0.2 0.2) square
              -- yellow armor
              Armor -> colored (rgb 0.8 0.8 0.2) square
              -- gray eye
              Eye -> colored (rgb 0.5 0.5 0.5) square
              -- dark blue food
              Food -> colored (rgb 0.2 0.2 0.8) square
              -- black empty
              Empty -> colored CodeWorld.black square
              -- black wall
              Wall -> colored (rgb 0 0 0) square

instance Drawable World where
  draw w = ((<>) `on` CodeWorld.pictures) (livingCells w) (nonLivingCells w)
    where
      livingCells = concatMap (map draw . anatomy) . Map.elems . organisms
      nonLivingCells = map draw . Map.elems . grid

instance Drawable Engine where
  draw = draw . world