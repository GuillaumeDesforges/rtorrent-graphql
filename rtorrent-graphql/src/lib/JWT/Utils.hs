{-# LANGUAGE OverloadedStrings #-}

-- | Some other functions that are handy for dealing with JWT in our application
module JWT.Utils where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value)
import Data.Map (Map)
import qualified Data.Text as TS
import Data.Text.Lazy (Text, fromStrict)
import Web.JWT (JWT, Signer, claims, unClaimsMap, unregisteredClaims)
import qualified Web.JWT as JWT

-- | Directly get unregistered claims from a decoded JWT token
getUnregisteredClaims :: JWT r -> Map TS.Text Value
getUnregisteredClaims = unClaimsMap . unregisteredClaims . claims

-- | Generate a basic JWT token for given signer and unregistered claims
getJwtTokenWithClaims :: MonadIO m => Signer -> Map TS.Text Value -> m Text
getJwtTokenWithClaims signer claimsMap = do
  return . fromStrict $ JWT.encodeSigned signer header claims
  where
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
          JWT.unregisteredClaims = JWT.ClaimsMap {JWT.unClaimsMap = claimsMap}
        }
