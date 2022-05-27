{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE RankNTypes     #-}

module HConduit
    ( yieldSum
    , yieldFile
    , main
    ) where

import Conduit (ConduitM (..), MonadIO (liftIO), mapM_C, runConduit, runConduitPure, runConduitRes,
                sinkFile, sourceFile, sumC, yield, yieldMany, (.|))

yieldFile :: IO ()
yieldFile = do
    -- create a source file
    writeFile "input.txt" "Static typic is the only thing that makes sense"

    runConduitRes
        $ sourceFile "input.txt"
        .| sinkFile "output.txt"

yieldSum :: IO ()
yieldSum = print
    $ runConduitPure
    $ yieldMany [1..10]
    .| sumC

loudYield :: Int -> ConduitM i Int IO ()
loudYield x = do
    liftIO $ putStrLn $ "yielding: " <> show x
    yield x

loudSinkNull :: ConduitM Int o IO ()
loudSinkNull = mapM_C \x -> putStrLn $ "awaited: " <> show x

main :: IO ()
main = runConduit
    $ mapM_ loudYield [1..10]
    .| loudSinkNull
