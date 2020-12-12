{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api (rootResolver)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Morpheus (interpreter)
import Web.Scotty (body, post, raw, scotty)

main :: IO ()
main = scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi =<< body)

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter rootResolver
