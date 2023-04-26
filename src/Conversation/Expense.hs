module Conversation.Expense (
  Expense (..),
  Amount (..),
  Description (..),
  Split (..),
  amountValue,
  descriptionText,
  peerPart,
) where

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


newtype Split = Split
  { myPart :: Integer
  }
  deriving (Show)


amountValue :: Amount -> Integer
amountValue (Amount n) = n


descriptionText :: Description -> String
descriptionText (Description s) = s


peerPart :: Split -> Integer
peerPart split = 100 - myPart split
