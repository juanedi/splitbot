{-# LANGUAGE TemplateHaskell #-}

module OpenAI (
  Handler,
  OpenAI.init,
  chat,
  systemMessage,
  userMessage,
  ChatModel (..),
  ChatMessage (..),
  ChatParams (..),
  Role (..),
) where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, withText)
import qualified Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics (Generic)
import Network.HTTP.Client (
  RequestBody (..),
  httpLbs,
  parseRequest_,
  requestBody,
  requestHeaders,
  responseBody,
  responseStatus,
 )
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types.Status (statusCode)


data Handler = Handler
  { http :: Http.Manager
  , token :: String
  }


init :: Http.Manager -> String -> Handler
init http_ token_ =
  Handler {http = http_, token = token_}


data ChatModel
  = GPT_4
  | GPT_4_0314
  | GPT_4_32k
  | GPT_4_32k_0314
  | GPT_3_5_Turbo
  | GPT_3_5_Turbo_0301


instance ToJSON ChatModel where
  toJSON model =
    toJSON
      ( case model of
          GPT_4 -> "gpt-4" :: String
          GPT_4_0314 -> "gpt-4-0314"
          GPT_4_32k -> "gpt-4-32k"
          GPT_4_32k_0314 -> "gpt-4-32k-0314"
          GPT_3_5_Turbo -> "gpt-3.5-turbo"
          GPT_3_5_Turbo_0301 -> "gpt-3.5-turbo-0301"
      )


data Role = System | User | Assistant deriving (Show)
$( deriveJSON
    (defaultOptions {constructorTagModifier = map toLower})
    ''Role
 )


data ChatMessage = ChatMessage
  { role :: Role
  , content :: Text
  }
  deriving (Generic, Show)


instance ToJSON ChatMessage
instance FromJSON ChatMessage


data ChatParams = ChatParams
  { model :: ChatModel
  , temperature :: Float
  , messages :: [ChatMessage]
  }
  deriving (Generic)


instance ToJSON ChatParams


data ApiError
  = UnexpectedStatusCode Int
  | GenericError String
  deriving (Show)


newtype ChatResponse = ChatResponse
  { choices :: NonEmpty Choice
  }
  deriving (Generic)


instance FromJSON ChatResponse


newtype Choice = Choice
  { message :: ChatMessage
  }
  deriving (Generic)


instance FromJSON Choice


chat :: Handler -> ChatParams -> IO (Either ApiError ChatMessage)
chat (Handler http token) params = do
  let request =
        (parseRequest_ "https://api.openai.com/v1/chat/completions")
          { Http.method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Authorization", BS.concat ["Bearer ", pack token])
              ]
          , requestBody = (RequestBodyLBS . Data.Aeson.encode) params
          }
  response <- httpLbs request http
  let statusCode_ = statusCode (responseStatus response)
      responseBody_ = responseBody response
  if statusCode_ == 200
    then case Data.Aeson.eitherDecode responseBody_ of
      Right chatResponse ->
        pure $ (Right . message . NonEmpty.head . choices) chatResponse
      Left err ->
        pure (Left (GenericError err))
    else do
      putStrLn ("Unexpected status code (" ++ show statusCode_ ++ "). Response body follows:\n" ++ show responseBody_)
      return (Left (UnexpectedStatusCode statusCode_))


systemMessage :: Text -> ChatMessage
systemMessage = ChatMessage System


userMessage :: Text -> ChatMessage
userMessage = ChatMessage User
