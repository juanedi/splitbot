module Telegram.Reply (
  Reply,
  apologizing,
  keyboardMarkup,
  plain,
  text,
  withOptions,
) where

import qualified Telegram.Api.SendMessage as SendMessage


data Reply
  = Reply
      String
      ReplyKeyboard
  deriving (Show)


data ReplyKeyboard
  = Normal
  | Options [String]
  deriving (Show)


apologizing :: Reply -> Reply
apologizing (Reply msgText keyboard) =
  Reply ("Sorry, I couldn't understand that. " ++ msgText) keyboard


plain :: String -> Reply
plain msgText = Reply msgText Normal


withOptions :: String -> [String] -> Reply
withOptions msg options = Reply msg (Options options)


text :: Reply -> String
text (Reply msgText _keyboard) = msgText


keyboardMarkup :: Reply -> SendMessage.ReplyMarkup
keyboardMarkup (Reply _msgText keyboard) =
  case keyboard of
    Normal -> SendMessage.ReplyKeyboardRemove
    Options opts ->
      SendMessage.InlineKeyboard
        (map (\o -> [o]) opts) -- display options as a column
        False
        True
