module Telegram.Message
  ( Username
  , Message
  , fromUpdate
  , chatId
  , username
  , text
  ) where

import           Telegram.Api (ChatId(..))
import           Telegram.Api.Update (Update)
import qualified Telegram.Api.Update as Update
import           Telegram.Username (Username)
import qualified Telegram.Username as Username

data Message = Message
  { chatId :: ChatId
  , username :: Username
  , text :: String
  }
  deriving Show

fromUpdate :: Update -> Message
fromUpdate update =
  let message = Update.message update
      user    = Update.from message
  in  Message
        { chatId   = ChatId (Update.id user)
        , username = Username.fromString (Update.username user)
        , text     = Update.text message
        }
