{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Permission where

import Control.Monad.Reader (MonadReader (reader))
import Data.String (IsString)

newtype Permission = Permission String deriving (Eq, Show, IsString)

type AuthenticatedContext = MonadReader [Permission]

hasPermission :: (AuthenticatedContext m) => Permission -> m Bool
hasPermission = reader . elem