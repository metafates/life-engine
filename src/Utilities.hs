module Utilities where

-- | Apply function to the tuple of 2 values of the same type
bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (a, b) = (f a, f b)

-- | Cantor pairing function. Returns unique number for each pair of 2 numbers
-- Order matters, e.g. cantor (a, b) != cantor (b, a)
cantor :: (Fractional a) => (a, a) -> a
cantor (a, b) = 1 / 2 * (a + b) * (a + b + 1) + b