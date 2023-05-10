module Conversation.Engines.GPT (Conversation.Engines.GPT.init, prompt) where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Conversation.Outcome (Outcome (..))
import Data.Text (Text)
import qualified Data.Text
import Dhall (ToDhall)
import qualified Dhall
import GHC.Generics (Generic)
import OpenAI (ChatMessage, ChatModel (..), ChatParams)
import qualified OpenAI
import qualified Telegram.Reply as Reply


newtype State = State
  { messages :: [ChatMessage]
  }


init :: OpenAI.Handler -> IO (Text -> IO Outcome)
init openAI = do
  stateVar <- newMVar (State [])
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


initialPrompt :: String
initialPrompt =
  "hello"


data PromptParams = PromptParams
  { userName :: String
  , partnerName :: String
  }
  deriving (Eq, Generic, Show)


instance ToDhall PromptParams


prompt :: String -> String -> IO Text
prompt userName partnerName = do
  func <- Dhall.input Dhall.auto "/Users/jedi/code/splitbot/src/Conversation/Engines/prompt.dhall"
  pure $ func (PromptParams {userName = userName, partnerName = partnerName})
