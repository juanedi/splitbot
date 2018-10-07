module Conversation.Parameters.Description
  ( Description(..)
  , ask
  , Conversation.Parameters.Description.read
  ) where

import Telegram.Api (Reply(..), ReplyKeyboard(..))

newtype Description = Description
  { text :: String
  }

ask :: Reply
ask = Reply "Please enter a description." Normal

read :: String -> Description
read = Description
