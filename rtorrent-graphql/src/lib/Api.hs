{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Api (rootResolver) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Writer (MonadTrans (lift))
import Data.Morpheus.Types
  ( GQLType,
    QUERY,
    Resolver,
    RootResolver (..),
    Undefined (..),
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Permission (AuthenticatedContext, Permission, hasPermission)

newtype Query m = Query
  { hello :: m Text
  }
  deriving (Generic, GQLType)

rootResolver :: (MonadIO m, AuthenticatedContext m) => RootResolver m () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {hello = resolveHello},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

hasPermissionOrFail :: (AuthenticatedContext m) => Permission -> Resolver QUERY () m a -> Resolver QUERY () m a
hasPermissionOrFail permission action = do
  isPermitted <- lift $ hasPermission permission
  if isPermitted then action else fail "Not Authorized"

resolveHello :: (MonadIO m, AuthenticatedContext m) => Resolver QUERY () m Text
resolveHello = do
  hasPermissionOrFail "query.hello" $ return "Hello world"
