{-# LANGUAGE RankNTypes, DataKinds, StandaloneKindSignatures, PolyKinds, TypeOperators, TypeFamilies, GADTs #-}
module TypeFamilies where

import Data.Kind (Type)
import Data.Functor.Identity (Identity)

append :: forall a.[a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- using type families
-- these are computations at the type leve

type Append :: forall a.[a] -> [a] -> [a]
type family Append xs ys where
    Append '[]    ys = ys
    Append (x:xs) ys = x : Append xs ys

type Pair :: Type -> Type
type Pair a = (a, a)

type S :: (Type -> Type) -> Type
data S k = MkS (k Bool) (k Integer)

-- type MaybeIf :: Bool -> Type -> Type
-- type family MaybeIf b t where
--     MaybeIf True t = Maybe t
--     MaybeIf False t = Identity t

type MaybeIf :: Bool -> Type -> Type
type family MaybeIf b where
    MaybeIf True = Maybe
    MaybeIf False = Identity

-- type families with GADTs

type HList :: [Type] -> Type
data HList xs where
    HNil :: HList '[]
    (:&) :: x -> HList xs -> HList (x : xs)
infixr 5 :&

hlength :: HList xs -> Int
hlength HNil = 0
hlength (_ :& xs) = 1 + hlength xs

-- Something to think about
-- typical reason one would reach for closed type families:
-- to implement operations on GADTs.
--
-- Another thing to note: evaluation of type families is not lazy.
