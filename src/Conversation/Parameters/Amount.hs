module Conversation.Parameters.Amount
  ( Amount(..)
  , ask
  , parse
  ) where

import Telegram.Reply (Reply)
import qualified Telegram.Reply as Reply
import Text.Read (readMaybe)

newtype Amount = Amount
  { value :: Integer
  }

ask :: Reply
ask = Reply.plain "How much?"

parse :: String -> Maybe Amount
parse str = fmap Amount $ (readMaybe str)
