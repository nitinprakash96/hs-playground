-- Chapter 3
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pearls.Saddleback where

mkMatrix :: (Num a, Enum a) => (a -> a -> a) -> a -> [[a]]
mkMatrix f z = [ [f x y | x <- [0..z]] | y <- [0..z]]

-- | Brute force approach
--
-- we know that @f(x , y) = z@ implies @x ≤ z@ and @y ≤ z@.
-- Hence we can define invert by a simple search of all possible pairs of
-- values
-- Involves (z + 1)^2 evaluations of @f@
--
-- >>> invertByJack (+) 4
invertByJack :: (Num a, Enum a, Eq a) => (a -> a -> a) -> a -> [(a, a)]
invertByJack f z = [ (x, y) | x <- [0..z], y <- [0..z], f x y == z ]

-- | Slightly optimised version of @invertByJack@
--
-- Since @f(x , y) ≥ x + y@ if @f@ is increasing, the search can be
-- confined to values on or below the diagonal of the square.
invertByTheo :: (Num a, Enum a, Eq a) => (a -> a -> a) -> a -> [(a, a)]
invertByTheo f z =
    [ (x, y) | x <- [0..z], y <- [0..z - x], f x y == z ]

-- | Optimisation done on top of @invertByTheo@
--
-- Assuming it doesn’t matter in which order the solutions are found,
-- if we start at the top-left corner @(0, z)@ of the square. At any
-- stage the search space is constrained to be a rectangle with top-left corner
-- @(u, v)@ and bottom-right corner @(z, 0)@.
--
-- >>> invertByAnne (+) 4
-- [(0,4),(1,3),(2,2),(3,1),(4,0)]
invertByAnne :: forall a. (Enum a, Eq a, Num a) => (a -> a -> a) -> a -> [(a, a)]
invertByAnne f z =
    find (0, z) f z
  where
      find :: (a, a) -> (a -> a -> a) -> a -> [(a, a)]
      find (u, v) f z = [(x, y) | x <- [u .. z], y <- [v, v - 1..0], f x y == z]

-- let's further improve ipon find defind above in the @where@ clause
