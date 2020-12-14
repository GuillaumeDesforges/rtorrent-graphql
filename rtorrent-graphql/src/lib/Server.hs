{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Api (rootResolver)
import Control.Monad.Except ()
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans (MonadTrans (lift))
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus (interpreter)
import Data.Text.Lazy (Text, fromStrict, unpack)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status400)
import Network.Wai (Application)
import Permission (Permission)
import qualified Web.JWT as JWT (Algorithm (HS256), JOSEHeader (JOSEHeader), JWTClaimsSet (JWTClaimsSet), alg, aud, cty, encodeSigned, exp, hmacSecret, iat, iss, jti, kid, nbf, sub, typ, unregisteredClaims)
import Web.Scotty (scottyApp)
import Web.Scotty.Trans (ActionT, body, json, post, raiseStatus, raw, setHeader)

-- TODO
defaultPermissions :: [Permission]
defaultPermissions = ["query.hello"]

data LoginRequestPayload = LoginRequestPayload {username :: String, password :: String} deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype LoginResponsePayload = LoginResponsePayload {token :: String} deriving (Eq, Show, Generic, FromJSON, ToJSON)

getJwtToken :: Text
getJwtToken = fromStrict $ JWT.encodeSigned secret header claims
  where
    secret = JWT.hmacSecret "my-little-secret"
    header = JWT.JOSEHeader {JWT.alg = Just JWT.HS256, JWT.typ = Just "JWT", JWT.cty = Nothing, JWT.kid = Nothing}
    claims =
      JWT.JWTClaimsSet
        { JWT.iss = Nothing,
          JWT.sub = Nothing,
          JWT.aud = Nothing,
          JWT.exp = Nothing,
          JWT.nbf = Nothing,
          JWT.iat = Nothing,
          JWT.jti = Nothing,
          JWT.unregisteredClaims = mempty
        }

getDefaultServer :: IO Application
getDefaultServer = scottyApp $ do
  -- Get access token
  post "/login" $ do
    maybeLoginRequestPayload :: Maybe LoginRequestPayload <- lift . return . decode =<< body
    case maybeLoginRequestPayload of
      Nothing -> raiseStatus status400 "Could not decode login request payload"
      Just loginRequestPayload -> json $ LoginResponsePayload {token = unpack getJwtToken}
  -- GraphQL endpoint
  post "/api" $ do
    -- Write response
    setHeader "Content-Type" "application/json"
    body >>= gqlApi defaultPermissions >>= raw

gqlApi :: (MonadIO m, Typeable m) => [Permission] -> ByteString -> ActionT Text m ByteString
gqlApi permissions query = runReaderT (interpreter rootResolver query) permissions
