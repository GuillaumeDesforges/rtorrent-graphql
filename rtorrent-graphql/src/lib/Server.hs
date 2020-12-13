{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Api (Permission, rootResolver)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Writer (WriterT (runWriterT))
import Data.Morpheus (interpreter)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import Network.Wai (Application)
import Web.Scotty (scottyApp)
import Web.Scotty.Trans (ActionT, body, json, post, raise)

getDefaultServer :: IO Application
getDefaultServer = scottyApp $ post "/api" $ body >>= gqlApi . decodeUtf8 >>= json

gqlApi :: (MonadIO m, Typeable m) => Text -> ActionT Text m Text
gqlApi query = do
  -- Get response data (embeded in a monad) from the graphql API resolution
  let resolveResult = interpreter rootResolver query
  -- Check all permissions
  (response, requiredPermissions :: [Permission]) <- runWriterT resolveResult
  let anyPermissionMisses = False -- TODO
  if anyPermissionMisses then raise "Missing permission on a queried resource" else undefined
  -- Return response
  lift . return $ response
