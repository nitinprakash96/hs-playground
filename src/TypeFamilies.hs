{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TypeFamilies where

import Data.Kind (Type)
import Data.Functor.Identity (Identity)
import GHC.TypeLits (Symbol, KnownSymbol (..), symbolVal)
import Data.Proxy ( Proxy (..) )
import Data.Aeson (Value (..))
import Data.Maybe (fromMaybe)

--------------------------------------------------
-- CLOSED TYPE FAMILIES
--------------------------------------------------

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

--------------------------------------------------
-- OPEN TYPE FAMILIES
--------------------------------------------------

-- type Label :: Type -> Symbol
-- type family Label t where
--     Label Double = "number"
--     Label String = "string"
--     Label Bool   = "bool"

-- the above is closed and obstructs the user from
-- decalring their own custom type(possible in another module)
-- we can enable that ommitting the @where@ keyword and making
-- it an open type family
type Label :: Type -> Symbol
type family Label t

type instance Label Double = "number"
type instance Label String = "string"
type instance Label Bool = "bool"

-- >>> label @Double
-- "number"
-- >>> label @Bool
-- "bool"
label :: forall t. KnownSymbol (Label t) => String
label = symbolVal (Proxy @(Label t))

data MyType = MT
type instance Label MyType = "mt"

-- >>> label @MyType
-- "mt"

-- Open type families seem much more extensible
-- but it comes at a cost
-- the equations of an open type family are not allowed
-- to overlap. But overlapping equations are often useful!

-- Open type family instances must be compatible.
-- Type family instances are compatible if at least
-- one of the following holds:
--
-- Their left-hand sides are apart (i.e. not overlapping)
-- Their left-hand sides unify with a substitution, under which the right-hand sides are equal.
