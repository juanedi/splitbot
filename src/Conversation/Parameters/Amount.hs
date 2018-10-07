module Conversation.Parameters.Amount
  ( Amount(..)
  , ask
  , parse
  ) where

import Telegram.Api (Reply(..), ReplyKeyboard(..))
import Text.Read (readMaybe)

newtype Amount = Amount
  { value :: Integer
  }

ask :: Reply
ask = Reply "How much?" Normal

parse :: String -> Maybe Amount
parse str = fmap Amount $ (readMaybe str)
