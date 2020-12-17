{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Api (rootResolver)
import Api.Login (LoginRequestPayload, LoginResponsePayload (LoginResponsePayload), token)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (toJSON)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import Data.Morpheus (interpreter)
import Data.Text.Lazy (Text, unpack)
import Data.Typeable (Typeable)
import JWT.Utils (getJwtTokenWithClaims)
import Network.Wai (Application)
import Permission (Permission, PermissionLevel (Admin), getPermissionLevelFromVerifiedJwtOrFail, getPermissionLevelPermissions)
import Web.JWT (Signer, hmacSecret)
import Web.Scotty (scottyApp)
import Web.Scotty.Action.Authorization (getAuthorizationHeaderBearerTokenOrFail, parseJwtOrFail)
import Web.Scotty.Action.Json (getJsonPayloadOrFail)
import Web.Scotty.Trans (ActionT, body, json, post, raw, setHeader)

-- | Secret for JWT
getAppSigner :: (MonadIO m) => m Signer
getAppSigner = pure $ hmacSecret "my-little-secret" -- FIXME get from somewhere (that's why signature is expected to be IO)

-- | Define routes and their behaviors
getDefaultServer :: IO Application
getDefaultServer = do
  -- Retrieve JWT private signer of this application
  signer <- getAppSigner
  -- Define Web app routing
  scottyApp $ do
    -- Get access token
    post "/login" $ do
      -- TODO get login payload, check it, and give appropriate permission level
      _ :: LoginRequestPayload <- getJsonPayloadOrFail
      bearerToken <- getJwtTokenWithClaims signer $ Map.fromList [("permissionLevel", toJSON Admin)]
      json $ LoginResponsePayload {token = unpack bearerToken}
    -- GraphQL endpoint
    post "/api" $ do
      -- Get permission level from JWT token present in Authorization header
      permissionLevel <- getPermissionLevelFromVerifiedJwtOrFail =<< parseJwtOrFail signer =<< getAuthorizationHeaderBearerTokenOrFail
      -- Compute permissions, get gql api resolvers with permissions
      let permissions = getPermissionLevelPermissions permissionLevel
          gqlApi = getGqlApi permissions
      -- Write response (Morpheus GraphQL returns a JSON string, we serve it raw with the right header)
      setHeader "Content-Type" "application/json"
      body >>= gqlApi >>= raw

-- | Get a GraphQL API. It resolves differently depending on the permissions given.
getGqlApi :: (MonadIO m, Typeable m) => [Permission] -> ByteString -> ActionT Text m ByteString
getGqlApi permissions query = runReaderT (interpreter rootResolver query) permissions
