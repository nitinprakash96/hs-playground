{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeApplications   #-}

module Generic where

-- 3 mechanisms: Typeable, Data, Generic

import Data.Data (Data)
import Data.Generics (everywhere, mkT)
import GHC.Generics ( Generic )
import Type.Reflection ((:~~:) (..), Typeable (..), eqTypeRep, typeOf, typeRep)


-- Typeble allows runtime type idenitification

doSomethingSpecialOnInts :: (Typeable a) => a -> a
doSomethingSpecialOnInts x
    | Just HRefl <- typeOf x `eqTypeRep` typeRep @Int = x + 1
    | otherwise = x

data Wurble = MkW Int Bool Int Double
  deriving (Show, Data)

add1 :: (Data a) => a -> a
add1 = everywhere (mkT ((+1) :: Int -> Int))

data Record = Record
    { field      :: Int
    , otherField :: Bool
    } deriving Generic
