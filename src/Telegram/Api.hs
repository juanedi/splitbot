module Telegram.Api where

import Data.Aeson (FromJSON, (.:), parseJSON, withObject)

data UpdateResponse = UpdateResponse
  { result :: [Telegram.Api.Update]
  } deriving (Show)

instance FromJSON UpdateResponse where
  parseJSON = withObject "update" $ \o -> fmap UpdateResponse (o .: "result")

data Update = Update
  { updateId :: Int
  , message :: Message
  } deriving (Show)

instance FromJSON Update where
  parseJSON =
    withObject "result" $ \o ->
      Update <$> (o .: "update_id") <*> (o .: "message")

data Message = Message
  { text :: String
  , from :: User
  } deriving (Show)

instance FromJSON Message where
  parseJSON =
    withObject "message" $ \o -> Message <$> o .: "text" <*> o .: "from"

data User = User
  { id :: Integer
  , username :: String
  } deriving (Show)

instance FromJSON User where
  parseJSON =
    withObject "user" $ \o -> User <$> (o .: "id") <*> (o .: "username")
