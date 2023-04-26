module Conversation.Parameters.Amount (
  Amount (..),
) where

import Telegram.Reply (Reply)
import qualified Telegram.Reply as Reply
import Text.Read (readMaybe)


newtype Amount = Amount
  { value :: Integer
  }
  deriving (Show)
