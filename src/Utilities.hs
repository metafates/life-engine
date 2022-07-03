module Utilities where

import System.Random (StdGen, randomR)

-- | Apply function to the tuple of 2 values of the same type
bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (a, b) = (f a, f b)

-- | Cantor pairing function. Returns unique number for each pair of 2 numbers
-- Order matters, e.g. cantor (a, b) != cantor (b, a)
cantor :: (Fractional a) => (a, a) -> a
cantor (a, b) = 1 / 2 * (a + b) * (a + b + 1) + b

-- | Same as cantor but for integral types
integralCantor :: (Integral a, Fractional b) => (a, a) -> b
integralCantor = cantor . bimap fromIntegral

relativeTo :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
relativeTo xs = zipWith vectorSum xs . repeat

corners :: [(Int, Int)]
corners = [(1, 1), (-1, -1), (1, -1), (-1, 1)]

adjacent :: [(Int, Int)]
adjacent = [(1, 0), (-1, 0), (0, 1), (0, -1)]

around :: [(Int, Int)]
around = adjacent ++ corners

vectorSum :: (Num a) => (a, a) -> (a, a) -> (a, a)
vectorSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

randomChoice :: StdGen -> [a] -> (a, StdGen)
randomChoice g xs = (xs !! i, g')
  where
    (i, g') = randomR (0, length xs - 1) g

shuffle :: StdGen -> [a] -> [a]
shuffle _ [] = []
shuffle gen list = randomElem : shuffle newGen newList
  where
    randomTuple = randomR (0, length list - 1) gen
    randomIndex = fst randomTuple
    newGen = snd randomTuple
    randomElem = list !! randomIndex
    newList = take randomIndex list ++ drop (randomIndex + 1) list

giveRandomElement :: StdGen -> [a] -> a
giveRandomElement gen list = list !! rand
  where
    n = length list
    (rand, _) = randomR (0, n -1) gen