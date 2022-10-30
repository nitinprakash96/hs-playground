-- Chapter 1

module Pearls.MinFree
    ( sort
    , countlist
    , minFree
    ) where

import Data.List (partition)
import GHC.Arr (Array (..), accumArray, assocs, elems)

-- Array based solution
minFree :: [Integer] -> Integer
minFree xs = head ([0..] \\ xs)

{-
The book writes this function as:
(\\) :: Eq a -> [a] -> [a] -> [a]
us\\vs = filter(∉ vs) us

where ∉ is a flipped version of @notElem@
-}
(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (notElem' vs) us

notElem' :: Eq a => [a] -> a -> Bool
notElem' a n = n `notElem` a

countlist :: [Integer] -> Array Integer Integer
countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
    where
        n = toInteger $ length xs

-- OFFTRACK
-- accumArray used in @countlist@ method can be used to sort a list of
-- numbers in linear time, provided the elements of the list all lie in some
-- known range (0, n).
--
-- λ> sort [5,1,2,3,0, 4]
-- [0,1,2,3,4,5]
--
-- λ> sort [5,1,2,3,0, 9]
-- *** Exception: Ix{Integer}.index: Index (9) out of range ((0,6))
sort :: [Integer] -> [Integer]
sort xs = concat [replicate (fromIntegral k) x | (x, k) <- assocs (countlist xs)]

-- | @countlist@ can be implemented using a combination of
-- @checklist@ and @search@ function as well.
-- and consequently @minFree@ will become @minFree = search . checklist@
--
-- The @checklist@ is a Boolean array with @n + 1@ slots, numbered
-- from 0 to n, whose initial entries are everywhere @False@.
-- For each element x in xs and provided @x ≤ n@ we set the array element
-- at position x to @True@. The smallest free number is then found as the position
-- of the first @False@ entry.
-- Runs in O(n) time.
checklist :: [Int] -> Array Int Bool
checklist xs =
    accumArray (||) False (0, n ) (zip (filter (<= n) xs) (repeat True))
        where n = length xs

-- The function search takes an array of Booleans, converts the array into a list
-- of Booleans and returns the length of the longest initial segment consisting
-- of True entries. This number will be the position of the first False entry.
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

minFree' :: [Int] -> Int
minFree' = search . checklist

-- Another solution could be by applying divide and conquer
minFree'' :: [Integer] -> Integer
minFree'' = minFrom'' 0

-- every element of xs is assumed to be greater than or equal to a
minFrom'' :: Integer -> [Integer] -> Integer
minFrom'' a xs = head ([a ..] \\ xs)


minFree''' :: [Integer] -> Integer
minFree''' xs = minFrom''' 0 (toInteger (length xs), xs)

-- The above can be further optimised by applying a few changes
-- 1. provided b is chosen so that both (length us) and (length vs)
--    are less than (length xs). We want b > a. And we would also like to
--    choose b so that the maximum of the lengths of us and vs is as small as
--    possible.
-- 2. We can avoid repeatedly computing length with a
--    simple data refinement, representing xs by a pair (length xs, xs)
minFrom''' :: Integer -> (Integer, [Integer]) -> Integer
minFrom''' a (n, xs)
    | n == 0 = a
    | m == b - a = minFrom''' b (n - m, vs)
    | otherwise = minFrom''' a (m, us)
    where
        (us, vs) = partition (< b) xs
        b = a + 1 + n `div` 2
        m = toInteger (length us)

-- When seeking a Θ(n) algorithm involving a list of n
-- elements, it is tempting to head at once for a method that processes each
-- element of the list in constant time, or at least in amortized constant time.
-- But a recursive process that performs Θ(n) processing steps in order to
-- reduce the problem to another instance of at most half the size is also good
-- enough.
