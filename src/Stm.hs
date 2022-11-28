{-# LANGUAGE NumericUnderscores #-}

module Stm
    ( makeCounter
    , makeCounterWithFork
    , writeIntoTvar
    , mvarArray
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar (..), modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar,
                                takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, modifyTVar', newTVar, newTVarIO, readTVarIO,
                                    writeTVar)
import Control.Monad (replicateM, replicateM_)
import Debug.Trace


-- TVars

makeCounter :: IO ()
makeCounter = do
    counter <- newTVarIO 0
    replicateM_ 10 $ do
        atomically $ modifyTVar counter (+ 1)
        readTVarIO counter >>= print


makeCounterWithFork :: IO ()
makeCounterWithFork = do
    counter <- newTVarIO 0
    replicateM_ 5 $ do
        _ <- forkIO $ atomically $ modifyTVar counter (+ 1)
        _ <- forkIO $ atomically $ modifyTVar counter (+ 4)
        -- _ <- forkIO $
        --     atomically $ do
        --         modifyTVar counter (+ 1)
        --         modifyTVar counter (+ 4)
        pure ()
    threadDelay 1_000
    readTVarIO counter >>= print

writeIntoTvar :: IO ()
writeIntoTvar = do
    name <- newTVarIO "Nitin"
    -- atomically $ modifyTVar name (++ " Prakash")
    atomically $ modifyTVar' name (++ " Prakash")
    readTVarIO name >>= print

-- MVars

mvarArray :: IO ()
mvarArray = do
    let n = 10
    m <- replicateM n newEmptyMVar
    repeats n m >>= print
  where
    repeats:: Int -> [MVar Int] -> IO [Int]
    repeats n xs = do
        mapM_ (\(e, x) -> putMVar e (x + 1)) $ zip xs [0..n]
        modifyMVar_ (head xs) (\_ -> pure 999)
        -- putMVar (head xs) 999
        mapM takeMVar xs
