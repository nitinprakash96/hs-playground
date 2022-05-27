{-# LANGUAGE RankNTypes #-}

module RankNTypes where

import Control.Monad.State
import System.Random

-- id' :: forall a. a -> a
-- id' x = x

-- type IdFunc = forall a. a -> a

-- type SomeInt = IdFunc -> Integer

-- someOtherInt :: SomeInt -> Integer
-- someOtherInt x = x id' + x id'

data Player = Player
  { pName :: String
  , pPos  :: (Double, Double)
  } deriving (Eq, Show)

randomPlayer
  :: (MonadIO m, MonadState g m, RandomGen g)
  => m Player
randomPlayer = undefined
