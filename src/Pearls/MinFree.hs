{-# LANGUAGE FlexibleContexts #-}
module Pearls.MinFree where

import GHC.Arr (Array (..), accumArray)

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
