module Conversation.Parameters.Payer
  ( ask
  , parse
  ) where

import Conversation.Parameters.Definitions
import Data.Char (toLower)
import Telegram.Api (Reply(..), ReplyKeyboard(..))

ask :: Reply
ask = Reply "Who paid?" (Options ["Me", "They"])

parse :: String -> Maybe Who
parse str = case (map toLower) str of
  "me"   -> Just Me
  "they" -> Just They
  _      -> Nothing
