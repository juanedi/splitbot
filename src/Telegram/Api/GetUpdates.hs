module Telegram.Api.GetUpdates where

import Data.Aeson (
    FromJSON,
    ToJSON,
    object,
    parseJSON,
    toJSON,
    withObject,
    (.:),
    (.=),
 )
import Telegram.Api.Update (Update)


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
    }
    deriving (Show)


instance FromJSON UpdateResponse where
    parseJSON = withObject "update" $ \o -> fmap UpdateResponse (o .: "result")
