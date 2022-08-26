{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

module GhcGenerics where

import GHC.Generics ((:*:) (..), (:+:) (..), C1, D1, Generic, K1 (..), M1 (..), Rep, S1, U1, V1,
                     from)


-- >>> :kind! (Rep User)
-- Compare this with toggling laziness and strict fields
-- using StrictData language extension and ~ and ! annotations
data User = User
    { uName :: !String
    , uId   :: !Int
    } deriving (Show, Generic)

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Generic

class Encode' f where
    encode' :: f p -> [Bool]

{-
data    V1        p                       -- lifted version of Empty
data    U1        p = U1                  -- lifted version of ()
data    (:+:) f g p = L1 (f p) | R1 (g p) -- lifted version of Either
data    (:*:) f g p = (f p) :*: (g p)     -- lifted version of (,)
newtype K1    i c p = K1 { unK1 :: c }    -- a container for a c
newtype M1  i t f p = M1 { unM1 :: f p }  -- a wrapper
-}

instance Encode' V1 where
    encode' x = case x of {}

instance Encode' U1 where
    encode' x = []

instance (Encode' f, Encode' g) => Encode' (f :+: g) where
    encode' (L1 x) = False : encode' x
    encode' (R1 x) = True : encode' x

instance (Encode' f, Encode' g) => Encode' (f :*: g) where
    encode' (x :*: y) = encode' x ++ encode' y

-- What is going on here?
-- src: https://hackage.haskell.org/package/base-4.16.2.0/docs/GHC-Generics.html
-- instance (Encode c) => Encode' (K1 i c) where
--     encode' (K1 x) = encode x

-- Unlike in K1, the instance for M1 refers to encode', not encode.
-- Why so?
instance (Encode' f) => Encode' (M1 i t f) where
    encode' (M1 x) = encode' x

class Encode a where
    encode :: a -> [Bool]
    default encode :: (Generic a, Encode' (Rep a)) => a -> [Bool]
    encode x = encode' (from x)

-- instance (Encode a) => Encode (Tree a) where
