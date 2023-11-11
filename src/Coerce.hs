{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations            #-}

module Coerce where

import Data.Coerce ( coerce )
import Data.Ord (Down (..))
import qualified Data.Set as S


newtype Age = MkAge Int
    deriving newtype Eq
    deriving Ord via (Down Int)

toAge :: Int -> Age
toAge n | 0 <= n
        , n <= 120 = coerce n
        | otherwise = error "bad age"

toAges :: [Int] -> [Age]
toAges = coerce

fromAge :: Age -> Int
fromAge = coerce

newtype Set a = MkSet [a] -- INVARIANT
type role Set nominal

minElem :: Set a -> a
minElem (MkSet (x: _)) = x
minElem _              = error "bad elemenet"

changeSet :: Set Age -> Set Int
changeSet = coerce
