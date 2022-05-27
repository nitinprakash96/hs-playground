module HResourceT
    ( main
    ) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (ResourceT (..), allocate, release, runResourceT)


danger :: Int -> IO ()
danger i = liftIO $
    putStrLn $ "5 divided by " ++ show i ++ " is " ++ show (5 `div` i)

somethingElse :: IO ()
somethingElse = liftIO $ putStrLn
    "This could take a long time, don't delay releasing the resource!"

main :: IO ()
main = do
    bracket
        (do
            putStrLn "Enter some number"
            readLn)
        (\i -> putStrLn $ "Freeing scarce resource: " ++ show i)
        danger
    somethingElse
