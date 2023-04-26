module Conversation.Parameters.Split (
  Split (..),
  Conversation.Parameters.Split.init,
  peerPart,
) where


newtype Split = Split
  { myPart :: Integer
  }
  deriving (Show)


init :: Integer -> Split
init = Split


peerPart :: Split -> Integer
peerPart split = 100 - myPart split
