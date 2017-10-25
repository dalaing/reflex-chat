{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Servant.API (
    MyAPI1
  , MyAPI2
  , UserName(..)
  , UserId(..)
  , MessageBody(..)
  ) where

import Servant.API
import Data.Aeson

newtype UserName =
  UserName { getUserName :: String }
  deriving (Eq, Ord, Show)

instance FromJSON UserName where
  parseJSON = withObject "User" $ \v ->
    UserName <$> v .: "name"

instance ToJSON UserName where
  toJSON (UserName n) =
        object ["name" .= n]

newtype UserId =
  UserId { getUserId :: Int }
  deriving (Eq, Ord, Show)

instance FromJSON UserId where
  parseJSON = withObject "User" $ \v ->
    UserId <$> v .: "id"

instance ToJSON UserId where
  toJSON (UserId i) =
    object ["id" .= i]

type MyAPI1 =
       "login" :> ReqBody '[JSON] UserName :> Post '[JSON] UserId
  :<|> "user" :> Capture "id" Int :> "file" :> Raw
  :<|> "user" :> Capture "id" Int :> Delete '[JSON] NoContent

newtype MessageBody =
  MessageBody { getMessageBody :: String}
  deriving (Eq, Ord, Show)

instance FromJSON MessageBody where
  parseJSON = withObject "Message" $ \v ->
    MessageBody <$> v .: "message"

instance ToJSON MessageBody where
  toJSON (MessageBody m) =
    object ["message" .= m]

type MyAPI2 =
       "login" :> ReqBody '[JSON] UserName :> Post '[JSON] UserId
  :<|> "user" :> Capture "id" Int :> "message" :> ReqBody '[JSON] MessageBody :> Post '[JSON] NoContent
  -- use servant / wai / websocket to handle notifications
  :<|> "user" :> Capture "id" Int :> "notifications" :> Raw
  :<|> "user" :> Capture "id" Int :> Delete '[JSON] NoContent

-- TODO a 3rd version, which gives you the choice of websockets or polling for notifications
