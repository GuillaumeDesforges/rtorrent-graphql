{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Control.Monad.IO.Class (MonadIO)
import Data.Morpheus.Types
  ( GQLRootResolver (..),
    GQLType,
    QUERY,
    Resolver,
    Undefined (..),
  )
import Data.Text (Text)
import GHC.Generics (Generic)

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver = Query {hello = resolveHello},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

newtype Query m = Query
  { hello :: m Text
  }
  deriving (Generic, GQLType)

resolveHello :: (MonadIO m) => Resolver QUERY () m Text
resolveHello = return "Hello world"
