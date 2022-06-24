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
data CellState =
  -- | A crucial cell which eats food in directly adjacent coordinates
    Mouth
  -- | Generates food in adjacent cells
  | Producer
  -- | Allows an organism to move and rotate randomly. 
  | Mover
  -- | Harms other organisms when it touches them in directly adjacent cells
  | Killer
  -- | Defends against the killer cell simply by ignoring its damage.
  | Armor
  -- | Allows an organism to see and alter its movement based on its perceptions
  | Eye
  -- | The cell can be eaten by an organism's mouth
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
data Organism = Organism
  { 
-- | The organism consists of cells (anatomy)
    anatomy :: [Cell],
-- | Health parameter defines amount of an organism's health
    health :: Int,
-- | foodCollected is a parameter that shows amount of food eaten by an organism. 
    direction :: Direction,
-- | foodCollected is a parameter that shows amount of food eaten by an organism. 
-- (once an organism eats more food than amount of its body cells it will reproduce)
    foodCollected :: Int,
-- | lifetime parameter shows how many frames a given organism has lived.
    lifetime :: Int,
-- | Probability of an organism's mutation (it can grow a new random cell, 
-- change an already created cell, lose a cell) If an organism mutates it has 10% chance 
-- to alter other properties (movement range, brain decisions, probability of mutation itself)
    mutationFactor :: Double,
-- | The length of an organism's life is equal to number of its cells multiplied by lifespan multiplier 
-- (100 by default but this number can alter because of mutations).
    lifespanFactor :: Int,
-- | The eye looks forward and and sees the first cells within a certain range (lookRange).    
    lookRange :: Int,
-- | randomGen is used for creating random direction for organisms (with a mover cell).
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
    fps :: Int,
    -- | random gen used to update world
    -- this is probably the worst way to do it, but it works
    gen :: StdGen
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