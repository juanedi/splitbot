module Conversation.Expense where

import Conversation.Parameters

data Expense = Expense
  { description :: Description
  , payer :: Who
  , amount :: Amount
  , split :: Split
  }
