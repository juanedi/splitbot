module Telegram.Api.SendMessage where

import Data.Aeson (ToJSON, (.=), object, toJSON)

data Message = Message
  { chatId :: Integer
  , text :: String
  , replyMarkup :: Maybe ReplyMarkup
  }

instance ToJSON Message where
  toJSON m =
    object $
    ["chat_id" .= (chatId m), "text" .= (text m)] <>
    case replyMarkup m of
      Nothing -> []
      Just m -> ["reply_markup" .= m]

data ReplyMarkup = InlineKeyboard
  { keyboard :: [[KeyboardButton]]
  , resizeKeyboard :: Bool
  , oneTimeKeyboard :: Bool
  }

instance ToJSON ReplyMarkup where
  toJSON m =
    object
      [ "keyboard" .= (keyboard m)
      , "resize_keyboard" .= (resizeKeyboard m)
      , "one_time_keyboard" .= (oneTimeKeyboard m)
      ]

type KeyboardButton = String
