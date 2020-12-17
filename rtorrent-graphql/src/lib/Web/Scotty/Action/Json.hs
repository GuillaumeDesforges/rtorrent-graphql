{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Scotty.Action.Json where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Aeson (FromJSON, decode)
import Data.Text.Lazy (Text)
import Network.HTTP.Types.Status (status400)
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (body, raiseStatus)

-- | Scotty action to get request body as JSON
getJsonPayloadOrFail :: (MonadFail m, MonadIO m, FromJSON a) => ActionT Text m a
getJsonPayloadOrFail = do
  maybeLoginRequestPayload :: Maybe a <- lift . return . decode =<< body
  case maybeLoginRequestPayload of
    Nothing -> raiseStatus status400 "Could not decode JSON payload"
    Just payload -> return payload
