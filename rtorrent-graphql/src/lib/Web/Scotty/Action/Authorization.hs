{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Scotty.Action.Authorization where

import Control.Monad.Except (MonadError (catchError))
import Control.Monad.IO.Class (MonadIO)
import Data.Text.Lazy (Text, toStrict)
import Network.HTTP.Types.Status (status403)
import Text.Regex.TDFA ((=~~))
import Web.JWT (JWT, Signer, VerifiedJWT, decodeAndVerifySignature)
import Web.Scotty.Trans (ActionT, header, raiseStatus)

extractBearerToken :: (MonadFail m) => Text -> m Text
extractBearerToken text = do
  -- order of regex result is (prefix, match, suffix, [subgroups])
  (_, _, _, token : _) :: (Text, Text, Text, [Text]) <- text =~~ ("Bearer (.+)" :: Text)
  return token

-- | Scotty action to get Bearer token from headers, or raises the right error to the client
getAuthorizationHeaderBearerTokenOrFail :: (MonadFail m, MonadIO m) => ActionT Text m Text
getAuthorizationHeaderBearerTokenOrFail = do
  authorizationHeaderValue <- header "Authorization"
  case authorizationHeaderValue of
    Nothing -> raiseStatus status403 "No authorization header in request"
    Just authorizationHeaderValue -> extractBearerToken authorizationHeaderValue `catchError` \_ -> raiseStatus status403 "No bearer token in authorization header"

-- | Helper to get a scotty action that decodes a JWT token, or raises the right error to the client
parseJwtOrFail :: Monad m => Signer -> Text -> ActionT Text m (JWT VerifiedJWT)
parseJwtOrFail signer token = case decodeAndVerifySignature signer (toStrict token) of
  Nothing -> raiseStatus status403 "Could not parse or verify JWT token"
  Just (verifiedJwt :: JWT VerifiedJWT) -> return verifiedJwt
