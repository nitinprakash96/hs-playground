{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module MutableArrays (mutArray) where

import Control.Concurrent (forkIO, threadDelay)
import Data.Array.IO (IOArray, MArray (..), getElems, readArray, writeArray)


mutArray :: IO ()
mutArray = do
    arr <- newArray (1,10) 0 :: IO (IOArray Int Int)
    a <- readArray arr 1
    writeArray arr 1 64
    b <- readArray arr 1
    getElems arr >>= print
