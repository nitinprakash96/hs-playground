-- Chapter 4
-- Objective: Let X and Y be two finite disjoint sets of elements over some ordered type
-- and of combined size greater than k . Consider the problem of computing
-- the k th smallest element of X ∪ Y . By definition, the k th smallest ele-
-- ment of a set is one for which there are exactly k elements smaller than it,
-- so the zeroth smallest is the smallest. How long does such a computation
-- take?

{-# LANGUAGE ScopedTypeVariables #-}

module Pearls.Selection where

-- >>> smallest 3 ([1, 2, 3, 9], [4, 10, 13, 33, 67])
-- 4
smallest :: forall a. Ord a => Int -> ([a], [a]) -> a
smallest k (xs, ys) = union (xs, ys) !! k
  where
    union :: ([a], [a]) -> [a]
    union (xs, []) = xs
    union ([], ys) = ys
    union (x:xs, y:ys)
        | x < y = x : union(xs, y:ys)
        | x > y = y : union(x:xs, ys)

-- Divide and conquer implementation
-- This implementation feels like an overkill (I don't understand the
-- proof either) which can be done the way @smallest@ function has
-- been defined above.
-- See https://github.com/derekmcloughlin/pearls/tree/master/chap04
-- for some additional info about this solution
-- >>> smallest' 3 ([1, 2, 3, 9], [4, 10, 13, 33, 67])
-- 4
smallest' :: forall a. Ord a => Int -> ([a], [a]) -> a
smallest' k ([], ws) = ws !! k
smallest' k (zs, []) = zs !! k
smallest' k (zs, ws) =
    case (a < b, k <= p + q) of
        (True, True)   -> smallest k (zs, us)
        (True, False)  -> smallest (k - p - 1) (ys, ws)
        (False, True)  -> smallest k (xs, ws)
        (False, False) -> smallest (k - q - 1) (zs, vs)
    where p = length zs `div` 2
          q = length ws `div` 2
          (xs, a : ys) = splitAt p zs
          (us, b : vs) = splitAt q ws
