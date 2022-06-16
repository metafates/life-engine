module Types where

import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (StdGen)
import Utilities (bimap)
import CodeWorld

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
    size :: Double,
    coords :: Coords
  }

-- | Organism
-- TODO: write comments to describe each field
data Organism = Organism
  { anatomy :: [Cell],
    health :: Int,
    moveDirection :: Maybe Direction,
    rotateDirection :: Maybe Direction,
    foodCollected :: Int,
    lifetime :: Double,
    mutationFactor :: Double,
    lifespanFactor :: Double,
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
        let (x', y') = bimap ((*) (size cell) . fromIntegral) (coords cell)
         in CodeWorld.translated x' y'

      size' = size cell
      square = CodeWorld.solidRectangle size' size'
      -- TODO: draw it in different colors
      figure = case state cell of
        Mouth     -> colored (RGBA 255 108 184 0.8) square -- pink
        Producer  -> colored (RGBA  45 255 117 0.8) square -- dark green
        Mover     -> colored (RGBA 142 104  68 0.8) square -- brown
        Killer    -> colored (RGBA 240  72  72 0.8) square -- red
        Armor     -> colored (RGBA 150 240 243 0.8) square -- light blue
        Eye       -> colored white                  square <> dilated 0.5 square
        Food      -> colored (RGBA 150 243 160 0.8) square -- light green
        Empty     -> colored black                  square
        Wall      -> colored (RGBA 112 112 112 0.8) square -- grey

instance Drawable World where
  draw w = ((<>) `on` CodeWorld.pictures) (livingCells w) (nonLivingCells w)
    where
      livingCells = concatMap (map draw . anatomy) . Map.elems . organisms
      nonLivingCells = map draw . Map.elems . grid

instance Drawable Engine where
  draw = draw . world