module Conversation.Expense (
  Expense (..),
  Amount (..),
  Description (..),
  amountValue,
  descriptionText,
) where

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


newtype Description = Description
  { text :: String
  }
  deriving (Show)


amountValue :: Amount -> Integer
amountValue (Amount n) = n


descriptionText :: Description -> String
descriptionText (Description s) = s
