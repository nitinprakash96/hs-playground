{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Pearls.Surpasser
    (msc
    ) where

-- Takes O(n^2) time
--
-- >>> msc "GENERATING" == 6
-- True
--
-- >>> msc [1,2,0,-1] == 1
-- True
--
-- >>> msc [1,2,3,4] == 0
-- False
msc :: Ord a => [a] -> Int
msc xs = maximum [scount z zs | z : zs <- tails xs]

-- | The value of scount x xs is the surpasser count of x in the list xs and tails
-- returns the nonempty tails of a nonempty list in decreasing order of length.
scount :: Ord a => a -> [a] -> Int
scount x xs = length (filter (> x) xs)

-- >>> tails "abc" == ["abc","bc","c"]
-- True
tails :: Ord a => [a] -> [[a]]
tails []     = []
tails (x:xs) = (x:xs) : tails xs

-- Divide an conquer solitution
-- Targetting complexity of O(n log n)

-- build a table of all surpasser count
-- >>> table "generating"
-- [('g',5),('e',6),('n',2),('e',5),('r',1),('a',4),('t',0),('i',1),('n',0),('g',0)]
table :: Ord a => [a] -> [(a, Int)]
table xs = [(z, scount z zs) | z : zs <- tails xs]

msc' :: Ord a => [a] -> Int
msc' xs = maximum . map snd $ table xs

-- | Builds the table but in ascending order of keys
-- >>> table' "generating"
-- [('a',4),('g',6),('e',6),('n',5),('e',5),('r',4),('t',0),('i',1),('n',0),('g',0)]
table' :: Ord a => [a] -> [(a, Int)]
table' [x] = [(x, 0)]
table' xs = join (m - n) (table ys) (table zs)
    where
        m = length xs
        n = m `div` 2
        (ys, zs) = splitAt n xs

-- TODO: Need to understand how we got here.
join :: (Num a, Ord b, Eq a) => a -> [(b, a)] -> [(b, a)] -> [(b, a)]
join 0 txs []   = txs
join n [] tys   = tys
join n txs@((x, c) : txs') tys@((y, d) : tys')
    | x < y = (x, c + n) : join n txs' tys
    | x >= y = (y, d) : join (n - 1) txs tys'
