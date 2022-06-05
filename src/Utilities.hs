module Utilities where

bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (a, b) = (f a, f b)