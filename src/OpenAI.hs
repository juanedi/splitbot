module OpenAI (
  Handler,
  OpenAI.init,
  chat,
  systemMessage,
  userMessage,
  ChatModel (..),
  ChatParams (..),
) where

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


data ChatMessage = ChatMessage
  { role :: Role
  , content :: String
  }


data Role = System | User | Assistant


data ChatParams = ChatParams
  { model :: ChatModel
  , temperature :: Float
  , messages :: [ChatMessage]
  }


chat :: Handler -> ChatParams -> IO ChatMessage
chat = undefined


systemMessage :: String -> ChatMessage
systemMessage = ChatMessage System


userMessage :: String -> ChatMessage
userMessage = ChatMessage User
