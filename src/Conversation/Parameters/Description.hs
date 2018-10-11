module Conversation.Parameters.Description
  ( Description(..)
  , ask
  , confirm
  , Conversation.Parameters.Description.read
  , readConfirmation
  ) where

import Telegram.Reply (Reply)
import qualified Telegram.Reply as Reply

newtype Description = Description
  { text :: String
  }

ask :: Reply
ask = Reply.plain "👋 Hey! Please enter a description for the expense report."

confirm :: Description -> Reply
confirm (Description description) = Reply.withOptions
  (concat
    [ "👋 Hey! We'll create an expense report for \""
    , description
    , "\". Is that correct?"
    ]
  )
  ["Yes", "No"]


read :: String -> Description
read = Description

readConfirmation :: String -> Bool
readConfirmation "Yes" = True
readConfirmation _     = False
