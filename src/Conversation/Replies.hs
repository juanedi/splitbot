module Conversation.Replies where

import Telegram (Reply(..), ReplyKeyboard(..))

askAmount :: Reply
askAmount = Reply "How much?" Normal

askWhoPaid :: Reply
askWhoPaid = Reply "Who paid?" (Options ["Me", "Them"])

askHowToSplit :: Reply
askHowToSplit =
  Reply
    "How will you split it?"
    (Options ["Evenly", "All on me", "All on them"])

done :: Reply
done = Reply "Done!" Normal

apologizing :: Reply -> Reply
apologizing (Reply text options) =
  Reply ("Sorry, I couldn't understand that. " ++ text) options
