module Types where

import qualified CodeWorld
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
  | Eye Direction
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
  draw world = ((<>) `on` CodeWorld.pictures) livingCells nonLivingCells
    where
      livingCells = concatMap (map draw . anatomy) (Map.elems $ organisms world)
      nonLivingCells = map draw $ Map.elems $ grid world
