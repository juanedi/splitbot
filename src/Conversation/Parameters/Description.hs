module Conversation.Parameters.Description (
  Description (..),
) where


newtype Description = Description
  { text :: String
  }
  deriving (Show)
