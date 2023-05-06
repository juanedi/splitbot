module Conversation.Engines.GPT (Conversation.Engines.GPT.init) where

import Conversation.Outcome (Outcome (..))
import qualified OpenAI
import qualified Telegram.Reply as Reply


init :: OpenAI.Handler -> IO (String -> IO Outcome)
init _openAI =
  pure onMessage


onMessage :: String -> IO Outcome
onMessage message =
  pure (Continue (Reply.plain "I don't want to talk to you"))
