{-# LANGUAGE OverloadedStrings #-}

module Server where

import Api (rootResolver)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Morpheus (interpreter)
import Network.Wai (Application)
import Web.Scotty (addHeader, body, post, raw, scottyApp)

getDefaultServer :: IO Application
getDefaultServer =
  scottyApp $
    post "/api" $ do
      addHeader "Content-Type" "application/json"
      body >>= (liftIO . gqlApi) >>= raw

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter rootResolver
