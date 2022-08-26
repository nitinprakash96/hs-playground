-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Wai where

import Blaze.ByteString.Builder (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import Control.Concurrent.MVar (MVar (..), modifyMVar, newMVar)
import Network.HTTP.Types (status200)
import Network.Wai (Application (..), Request (..), Response (..), ResponseReceived (..),
                    responseBuilder, responseLBS)
import Network.Wai.Handler.Warp (run)

-- application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- application :: Application
-- application _ respond = respond $
--   responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

application :: MVar Int -> Application
application countRef _ respond = do
    modifyMVar countRef $ \count_ -> do
        let -- count_ = count + 1
            msg = fromByteString "You are visitor number: " <> fromShow count_
        responseReived <- respond $
            responseBuilder
                status200 [("Content-Type", "text/plain")] msg
        pure (count_, responseReived)

main :: IO ()
main = do
    visitorCount <- newMVar 0
    run 3000 $ application visitorCount
