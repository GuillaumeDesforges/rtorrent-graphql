{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Writer (MonadTrans (lift), WriterT, tell)
import Data.Morpheus.Types
  ( GQLType,
    QUERY,
    Resolver,
    RootResolver (..),
    Undefined (..),
  )
import Data.Text (Text)
import GHC.Generics (Generic)

type ResolverMonad m = AuthT m

rootResolver :: (MonadIO m) => RootResolver (ResolverMonad m) () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {hello = resolveHello},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

newtype Query m = Query
  { hello :: m Text
  }
  deriving (Generic, GQLType)

resolveHello :: (MonadIO m) => Resolver QUERY () (ResolverMonad m) Text
resolveHello = do
  lift $ hasPermission (Permission "query.hello")
  return "Hello world"

newtype Permission = Permission {permission :: String}

type AuthT = WriterT [Permission]

hasPermission :: (Monad m) => Permission -> AuthT m ()
hasPermission permission = tell [permission]
