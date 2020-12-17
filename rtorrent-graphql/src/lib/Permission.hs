{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Permission where

import Control.Monad.Reader (MonadReader (reader))
import Data.Aeson (FromJSON, Result (Error, Success), ToJSON, fromJSON)
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types (status403)
import qualified Web.JWT as JWT
import Web.Scotty.Trans (ActionT, raiseStatus)

-- | Define permissions in the app. They are strings with dots, e.g. 'admin.getAllUsers'
newtype Permission = Permission String deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance IsString Permission where
  fromString = Permission

-- | Allow to read the set of permissions given in the context
type AuthenticatedContext = MonadReader [Permission]

-- Helper function
hasPermission :: (AuthenticatedContext m) => Permission -> m Bool
hasPermission = reader . elem

-- | Define here the permission levels in the app
data PermissionLevel = Admin deriving (Eq, Show, Enum, Generic, ToJSON, FromJSON)

-- | Helper that gives a Scotty action to read permission level from a JWT token, or raises the right error to the client
getPermissionLevelFromVerifiedJwtOrFail :: Monad m => JWT.JWT JWT.VerifiedJWT -> ActionT Text m PermissionLevel
getPermissionLevelFromVerifiedJwtOrFail verifiedToken = do
  let maybePermissionLevel = fmap fromJSON . Map.lookup "permissionLevel" . JWT.unClaimsMap . JWT.unregisteredClaims . JWT.claims $ verifiedToken
  case maybePermissionLevel of
    Nothing -> raiseStatus status403 "No permission level in signed JWT. This should never happen, please contact administrator."
    Just (Error _) -> raiseStatus status403 "Bad permission level in signed JWT. This should never happen, please contact administrator."
    Just (Success permissionLevel) -> return permissionLevel

-- TODO make a real permission level system with attributions
getPermissionLevelPermissions :: PermissionLevel -> [Permission]
getPermissionLevelPermissions Admin = ["query.hello"]
