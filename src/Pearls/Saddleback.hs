-- Chapter 3
-- Objective: To find a value of @invert f z@ which is a list
-- of all pairs (x, y) satisfying @f (x , y) = z@. You can assume that f is strictly
-- increasing in each argument, but nothing else.

-- TODO: visit this chapter a few more times

-- Reference: www.cs.geneseo.edu/~baldwin/math-thinking/saddleback.html


{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Pearls.Saddleback where

-- >>> mkMatrix (+) 4
-- [[0,1,2,3,4],[1,2,3,4,5],[2,3,4,5,6],[3,4,5,6,7],[4,5,6,7,8]]
mkMatrix :: (Num a, Enum a) => (a -> a -> a) -> a -> [[a]]
mkMatrix f z = [ [f x y | x <- [0..z]] | y <- [0..z] ]

-- | Brute force approach
--
-- we know that @f(x , y) = z@ implies @x ≤ z@ and @y ≤ z@.
-- Hence we can define invert by a simple search of all possible pairs of
-- values
-- Involves (z + 1)^2 evaluations of @f@
--
-- >>> invertByJack (+) 4
-- [(0,4),(1,3),(2,2),(3,1),(4,0)]
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
invertByAnne :: forall a. Integral a => (a -> a -> a) -> a -> [(a, a)]
invertByAnne f z =
    -- find (0, z) f z
    find' (0, z) f z
  where
      find :: (a, a) -> (a -> a -> a) -> a -> [(a, a)]
      find (u, v) f z = [(x, y) | x <- [u .. z], y <- [v, v - 1..0], f x y == z]

      -- let's further improve upon @find@ defind above
      -- In the worst case, traverses the perimeter of the square from the
      -- top-left corner to the bottom-right corner, it performs 2z + 1 evaluations
      -- of f.
      -- In the best case, when find proceeds directly to either the bottom or
       -- rightmost boundary, it requires only z + 1 evaluations.
      find' :: (a, a) -> (a -> a -> a) -> a -> [(a, a)]
      find' (u, v) f z
          | u > z || v < 0 = []
          | z' < z = find' (u + 1, v) f z
          | z' == z = (u, v) : find' (u + 1, v - 1) f z
          | z' > z = find' (u, v - 1) f z
          where
            z' = f u v

-- What if we apply binary search?
-- m = maximum (filter (\y → f (0, y) <= z ) [0 .. z])
-- n = maximum (filter (\x -> f (x, 9) <= z) [0 .. z])
--
-- Then we can define invert f z = find (0, m) f z
-- This way, rather than searching a (z+1) x (z+1) square we can get away
-- with searching an (m+1) × (n+1) rectangle.
--
-- The reason the above works in because we're already given that
-- top left corner is (0, z) and bottom right is (z, 0).
-- We take advantage of this constraint.

-- >>> invertImprovedByTheo (+) 3
-- [(0,3),(1,2),(2,1),(3,0)]
invertImprovedByTheo :: forall a. Integral a => (a -> a -> a) -> a -> [(a, a)]
invertImprovedByTheo f z = find (0, m) f z
  where
    m = bsearch (\y -> f 0 y) (-1, z + 1) z

    bsearch :: (a -> a) -> (a, a) -> a -> a
    bsearch g (a, b) z
        | a + 1 == b = a
        | g m <= z   = bsearch g (m, b) z
        | otherwise  = bsearch g (a, m) z
        where
          m = (a + b) `div` 2

    -- exactly the same form invertByAnne, except that the first guard becomes (u > n || v < 0)
    find (u, v) f z
        | u > n || n < 0   = []
        | z' < z           = find (u + 1, v) f z
        | z' == z          = (u, v) : find (u + 1, v - 1) f z
        | z' > z           = find (u, v - 1) f z
        where
          z' = f u v
          n = maximum (filter (\x -> f x 0 <= z) [0 .. z ])
