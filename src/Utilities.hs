module Utilities where

-- | Apply function to the tuple of 2 values of the same type
bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (a, b) = (f a, f b)

-- | Cantor pairing function. Returns unique number for each pair of 2 numbers
-- Order matters, e.g. cantor (a, b) != cantor (b, a)
cantor :: (Fractional a) => (a, a) -> a
cantor (a, b) = 1 / 2 * (a + b) * (a + b + 1) + b

corners :: [(Int, Int)]
corners = [(1, 1), (-1, -1), (1, -1), (-1, 1)]

adjacent :: [(Int, Int)]
adjacent = [(1, 0), (-1, 0), (0, 1), (0, -1)]

around :: [(Int, Int)]
around = adjacent ++ corners

vectorSum :: (Num a) => (a, a) -> (a, a) -> (a, a)
vectorSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)