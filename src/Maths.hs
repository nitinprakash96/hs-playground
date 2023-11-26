{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Maths where

-- import GHC.Exts


-- data MyMaybe a = MyJust a | MyNothing deriving (Eq, Show)

data Parity = Even | Odd

instance Num Parity where
  Even + Even = Even
  Even + Odd = Odd
  Odd  + Odd = Even
  Odd  + Even = Odd

  p1 - p2 = p1 + p2

  negate p = p
  abs p = p
  signum p = p

  Odd * Odd = Odd
  _ * _ = Even

  fromInteger p | even p = Even
                | otherwise = Odd

data ParityInteger = MkPI Parity Integer

piFromInteger :: Integer -> ParityInteger
piFromInteger n | even n = MkPI Even n
                | otherwise = MkPI Odd n

add :: ParityInteger -> ParityInteger -> ParityInteger
add (MkPI p1 i1) (MkPI p2 i2) = MkPI (p1 + p2) (i1 + i2)

bigPi :: ParityInteger
bigPi = go (piFromInteger 0) [1 .. 100000]
  where
    go :: ParityInteger -> [Integer] -> ParityInteger
    go acc []       = acc
    go acc (i : is) = go (add acc (piFromInteger i)) is

integerFromPi :: ParityInteger -> Integer
integerFromPi (MkPI _ i) = i

newtype ParityInteger# = MkPI# (# Parity, Integer #)


piFromInteger# :: Integer -> ParityInteger#
piFromInteger# n | even n = MkPI# (# Even, n #)
                | otherwise = MkPI# (# Odd, n #)

add# :: ParityInteger# -> ParityInteger# -> ParityInteger#
add# (MkPI# (# p1, i1 #)) (MkPI# (# p2, i2 #))
  = MkPI# (# p1 + p2, i1 + i2 #)

bigPi# :: () -> ParityInteger#
bigPi# _ = go (piFromInteger# 0) [1 .. 100000]
  where
    go :: ParityInteger# -> [Integer] -> ParityInteger#
    go acc []       = acc
    go acc (i : is) = go (add# acc (piFromInteger# i)) is


integerFromPi# :: ParityInteger# -> Integer
integerFromPi# (MkPI# (# _, i #)) = i


main :: IO ()
main = print (integerFromPi# (bigPi# ()))
