module Conversation.Expense where

import Conversation.Parameters.Amount (Amount)
import Conversation.Parameters.Description (Description)
import Conversation.Parameters.Split (Split)
import Conversation.Parameters.Who (Who)

data Expense = Expense
  { description :: Description
  , payer :: Who
  , amount :: Amount
  , split :: Split
  }
  deriving Show
