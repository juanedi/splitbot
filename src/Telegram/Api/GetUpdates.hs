module Telegram.Api.GetUpdates where

import Data.Aeson
  ( FromJSON
  , ToJSON
  , (.:)
  , (.=)
  , object
  , parseJSON
  , toJSON
  , withObject
  )

data Request = Request
  { timeout :: Integer
  , limit :: Int
  , allowedUpdates :: [String]
  , offset :: Maybe Integer
  }

instance ToJSON Request where
  toJSON r =
    object $
    [ "timeout" .= timeout r
    , "limit" .= limit r
    , "allowed_updates" .= allowedUpdates r
    , "offset" .= offset r
    ]

data UpdateResponse = UpdateResponse
  { result :: [Update]
  } deriving (Show)

instance FromJSON UpdateResponse where
  parseJSON = withObject "update" $ \o -> fmap UpdateResponse (o .: "result")

data Update = Update
  { updateId :: Integer
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
