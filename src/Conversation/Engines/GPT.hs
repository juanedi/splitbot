module Conversation.Engines.GPT (Conversation.Engines.GPT.init, PromptParams (..)) where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Conversation.Outcome (Outcome (..))
import Data.Text (Text)
import qualified Data.Text
import Dhall (ToDhall)
import qualified Dhall
import GHC.Generics (Generic)
import OpenAI (ChatMessage, ChatModel (..), ChatParams)
import qualified OpenAI
import System.FilePath.Posix (FilePath)
import qualified Telegram.Reply as Reply


newtype State = State
  { messages :: [ChatMessage]
  }


data PromptParams = PromptParams
  { userName :: String
  , partnerName :: String
  }
  deriving (Eq, Generic, Show)


instance ToDhall PromptParams


init :: OpenAI.Handler -> FilePath -> PromptParams -> IO (Text -> IO Outcome)
init openAI promptTemplatePath promptParams = do
  promptTemplate <- Dhall.input Dhall.auto (Data.Text.pack promptTemplatePath)

  let prompt =
        OpenAI.ChatMessage
          { OpenAI.content = promptTemplate promptParams
          , OpenAI.role = OpenAI.User
          }

  stateVar <- newMVar (State [prompt])

  pure $ \message -> do
    modifyMVar
      stateVar
      (onMessage openAI message)


onMessage :: OpenAI.Handler -> Text -> State -> IO (State, Outcome)
onMessage openAI message state = do
  let withUserMessage =
        addMessage
          (OpenAI.ChatMessage {OpenAI.content = message, OpenAI.role = OpenAI.User})
          (messages state)
  result <-
    OpenAI.chat
      openAI
      ( OpenAI.ChatParams
          { OpenAI.model = GPT_3_5_Turbo
          , OpenAI.temperature = 0.7
          , OpenAI.messages = withUserMessage
          }
      )
  case result of
    Left err ->
      pure
        (state, Continue (Reply.plain ("Ooops something went wrong 😅" ++ show err)))
    Right botMessage ->
      pure
        ( State (addMessage botMessage (messages state))
        , Continue (Reply.plain (Data.Text.unpack (OpenAI.content botMessage)))
        )


addMessage :: ChatMessage -> [ChatMessage] -> [ChatMessage]
addMessage message messages =
  messages ++ [message]
