module Conversation.Parameters
  ( Amount(..)
  , Payer(..)
  , Split(..)
  , readAmount
  , readPayer
  , readSplit
  ) where

import Data.Char (toLower)
import Text.Read (readMaybe)

newtype Amount = Amount
  { value :: Int
  }

data Payer
  = Me
  | They

newtype Split = Split
  { myPart :: Integer
  } deriving (Show)

readAmount :: String -> Maybe Amount
readAmount str = fmap Amount $ (readMaybe str)

readPayer :: String -> Maybe Payer
readPayer str =
  case (map toLower) str -- TODO: this should be defined in the same place as the keyboard options
        of
    "me" -> Just Me
    "they" -> Just They
    _ -> Nothing

readSplit :: String -> Maybe Split
readSplit str =
  case (map toLower) str -- TODO: this should be defined in the same place as the keyboard options
        of
    "evenly" -> Just $ Split 50
    "all on me" -> Just $ Split 100
    "all on them" -> Just $ Split 0
    _
      -- TODO: parse things such as "40% me / %30 them"
     -> Nothing
