module Conversation.Outcome (Outcome (..)) where

import Conversation.Expense (Expense)
import Telegram.Reply (Reply)


data Outcome
  = Continue Reply
  | Terminate Reply
  | Done Expense
  deriving (Show)
