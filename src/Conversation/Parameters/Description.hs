module Conversation.Parameters.Description
  ( Description(..)
  , ask
  , confirm
  , Conversation.Parameters.Description.read
  , readConfirmation
  ) where

import Telegram.Api (Reply(..), ReplyKeyboard(..))

newtype Description = Description
  { text :: String
  }

ask :: Reply
ask = Reply "👋 Hey! Please enter a description for the expense report." Normal

confirm :: Description -> Reply
confirm (Description description) = Reply
  (concat
    [ "👋 Hey! We'll create an expense report for \""
    , description
    , "\". Is that correct?"
    ]
  )
  (Options ["Yes", "No"])


read :: String -> Description
read = Description

readConfirmation :: String -> Bool
readConfirmation "Yes" = True
readConfirmation _     = False
