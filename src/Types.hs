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
-- TODO: write comments to describe each field
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
        let colored = CodeWorld.colored
            square = CodeWorld.solidRectangle cellSize cellSize
            rgba = CodeWorld.RGBA
            white = CodeWorld.white
            black = CodeWorld.black
            pink = rgba 255 108 184 0.8
            darkGreen = rgba 45 255 117 0.8
            brown = rgba 142 104 68 0.8
            red = rgba 240 72 72 0.8
            lightBlue = rgba 150 240 243 0.8
            lightGreen = rgba 150 243 160 0.8
            gray = rgba 112 112 112 0.8
            dilated = CodeWorld.dilated
         in case state cell of
              Mouth -> colored pink square
              Producer -> colored darkGreen square
              Mover -> colored brown square
              Killer -> colored red square
              Armor -> colored lightBlue square
              Eye -> colored white square <> dilated 0.5 square
              Food -> colored lightGreen square
              Empty -> colored black square
              Wall -> colored gray square

instance Drawable World where
  draw w = ((<>) `on` CodeWorld.pictures) (livingCells w) (nonLivingCells w)
    where
      livingCells = concatMap (map draw . anatomy) . Map.elems . organisms
      nonLivingCells = map draw . Map.elems . grid

instance Drawable Engine where
  draw = draw . world