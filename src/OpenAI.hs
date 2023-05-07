module OpenAI (
  Handler,
  OpenAI.init,
  chat,
  systemMessage,
  userMessage,
  ChatModel (..),
  ChatParams (..),
) where

import Data.Aeson (ToJSON, toJSON)
import qualified Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
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
    toJSON $
      ( case model of
          GPT_4 -> "gpt-4" :: String
          GPT_4_0314 -> "gpt-4-0314"
          GPT_4_32k -> "gpt-4-32k"
          GPT_4_32k_0314 -> "gpt-4-32k-0314"
          GPT_3_5_Turbo -> "gpt-3-5-turbo"
          GPT_3_5_Turbo_0301 -> "gpt-3-5-turbo-0301"
      )


data ChatMessage = ChatMessage
  { role :: Role
  , content :: String
  }
  deriving (Generic)


instance ToJSON ChatMessage


data Role = System | User | Assistant


instance ToJSON Role where
  toJSON role =
    toJSON $
      ( case role of
          System -> "system" :: String
          User -> "user"
          Assistant -> "assistant"
      )


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


data ChatResponse = ChatResponse
  { choces :: [Choice]
  }


data Choice = Choice
  { message :: ChatMessage
  }
  deriving (Generic)


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
  let status = (statusCode . responseStatus) response
  if status == 200
    then -- TODO
      undefined
    else return (Left (UnexpectedStatusCode status))


systemMessage :: String -> ChatMessage
systemMessage = ChatMessage System


userMessage :: String -> ChatMessage
userMessage = ChatMessage User
