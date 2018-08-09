module Telegram.Api.SendMessage where

import Data.Aeson (ToJSON, (.=), object, toJSON)

data Message = Message
  { chatId :: Integer
  , text :: String
  , replyMarkup :: ReplyMarkup
  }

instance ToJSON Message where
  toJSON m =
    object $
    [ "chat_id" .= (chatId m)
    , "text" .= (text m)
    , "reply_markup" .= (replyMarkup m)
    ]

data ReplyMarkup
  = InlineKeyboard { keyboard :: [[KeyboardButton]]
                   , resizeKeyboard :: Bool
                   , oneTimeKeyboard :: Bool }
  | ReplyKeyboardRemove

instance ToJSON ReplyMarkup where
  toJSON m =
    case m of
      InlineKeyboard keyboard resizeKeyboard oneTimeKeyboard ->
        object
          [ "keyboard" .= keyboard
          , "resize_keyboard" .= resizeKeyboard
          , "one_time_keyboard" .= oneTimeKeyboard
          ]
      ReplyKeyboardRemove -> object ["remove_keyboard" .= True]

type KeyboardButton = String
