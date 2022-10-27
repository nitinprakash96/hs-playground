{-# LANGUAGE NumericUnderscores #-}

module Stm
    ( makeCounter
    , makeCounterWithFork
    , writeIntoTvar
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVar, newTVarIO, readTVarIO,
                               writeTVar)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Monad (replicateM_)
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
