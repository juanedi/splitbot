module Conversation.Engines.GPT (Conversation.Engines.GPT.init) where

import Conversation.Outcome (Outcome (..))
import qualified Telegram.Reply as Reply


init :: String -> IO (String -> IO Outcome)
init _token =
  pure onMessage


onMessage :: String -> IO Outcome
onMessage message =
  pure (Continue (Reply.plain "I don't want to talk to you"))
