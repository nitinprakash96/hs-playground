-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Wai where

import Blaze.ByteString.Builder (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseBuilder)
import Network.Wai.Handler.Warp (run)

-- application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- application :: Application
-- application _ respond = respond $
--   responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

{-
In theory,
- Using lazy I/O, we can have a value which represents the entire contents of a file, yet
  only occupies a small memory footprint.
  Therefore, this seems like an ideal option.

In practice,
 - lazy byte strings are wonderful for generating "pure" values
 - the lazy I/O necessary to read a file introduces some non-determinism into programs.
When serving thousands of small files a second, the limiting factor is not memory, but file handles.
Using lazy I/O, file handles may not be freed immediately, leading to resource exhaustion.
-}

application :: MVar Int -> Application
application countRef _ respond = do
    modifyMVar countRef $ \count -> do
        let count_ = count + 1
            msg = fromByteString "You are visitor number: " <> fromShow count_
        responseReived <- respond $
            responseBuilder
                status200 [("Content-Type", "text/plain")] msg
        pure (count_, responseReived)

main :: IO ()
main = do
    visitorCount <- newMVar 0
    run 3000 $ application visitorCount
