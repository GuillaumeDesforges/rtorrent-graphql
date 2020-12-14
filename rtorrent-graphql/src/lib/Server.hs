{-# LANGUAGE OverloadedStrings #-}

module Server where

import Api (rootResolver)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus (interpreter)
import Data.Text.Lazy (Text)
import Data.Typeable (Typeable)
import Network.Wai (Application)
import Permission (Permission)
import Web.Scotty (scottyApp)
import Web.Scotty.Trans (ActionT, body, post, raw, setHeader)

-- TODO
defaultPermissions :: [Permission]
defaultPermissions = ["query.hello"]

getDefaultServer :: IO Application
getDefaultServer = scottyApp $
  post "/api" $ do
    setHeader "Content-Type" "application/json"
    body >>= gqlApi defaultPermissions >>= raw

gqlApi :: (MonadIO m, Typeable m) => [Permission] -> ByteString -> ActionT Text m ByteString
gqlApi permissions query = runReaderT (interpreter rootResolver query) permissions
