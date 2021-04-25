module Conversation.Parameters.Payer (
  ask,
  parse,
) where

import Conversation.Parameters.Who
import Data.Char (toLower)
import Telegram.Reply (Reply)
import qualified Telegram.Reply as Reply


ask :: Reply
ask = Reply.withOptions "Who paid?" ["Me", "They"]


parse :: String -> Maybe Who
parse str =
  case (map toLower) str of
    "me" -> Just Me
    "they" -> Just They
    _ -> Nothing
