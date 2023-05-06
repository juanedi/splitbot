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
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as Http


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


chat :: Handler -> ChatParams -> IO ChatMessage
chat = undefined


systemMessage :: String -> ChatMessage
systemMessage = ChatMessage System


userMessage :: String -> ChatMessage
userMessage = ChatMessage User
