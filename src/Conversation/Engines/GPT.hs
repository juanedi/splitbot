module Conversation.Engines.GPT (Conversation.Engines.GPT.init) where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Conversation.Outcome (Outcome (..))
import OpenAI (ChatMessage, ChatModel (..), ChatParams)
import qualified OpenAI
import qualified Telegram.Reply as Reply


data State = State
  { messages :: [ChatMessage]
  }


init :: OpenAI.Handler -> IO (String -> IO Outcome)
init openAI = do
  stateVar <- newMVar (State [])
  pure $ \message -> do
    modifyMVar
      stateVar
      (onMessage openAI message)


onMessage :: OpenAI.Handler -> String -> State -> IO (State, Outcome)
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
        , Continue (Reply.plain (OpenAI.content botMessage))
        )


addMessage :: ChatMessage -> [ChatMessage] -> [ChatMessage]
addMessage message messages =
  messages ++ [message]
