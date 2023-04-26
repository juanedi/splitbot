module Conversation.Expense (
  Expense (..),
  Amount (..),
  amountValue,
) where

import Conversation.Parameters.Description (Description)
import Conversation.Parameters.Split (Split)
import Conversation.Parameters.Who (Who)


data Expense = Expense
  { description :: Description
  , payer :: Who
  , amount :: Amount
  , split :: Split
  }
  deriving (Show)


newtype Amount = Amount
  { value :: Integer
  }
  deriving (Show)


amountValue :: Amount -> Integer
amountValue (Amount n) = n
