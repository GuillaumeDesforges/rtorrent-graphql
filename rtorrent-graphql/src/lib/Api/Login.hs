{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Login where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data LoginRequestPayload = LoginRequestPayload {username :: String, password :: String} deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype LoginResponsePayload = LoginResponsePayload {token :: String} deriving (Eq, Show, Generic, FromJSON, ToJSON)
