module Conversation.Replies where

data Reply = Reply
  { text :: String
  , options :: Maybe [String]
  }

askAmount :: Reply
askAmount = Reply "How much?" Nothing

askWhoPaid :: Reply
askWhoPaid = Reply "Who paid?" (Just ["Me", "Them"])

askHowToSplit :: Reply
askHowToSplit =
  Reply "How will you split it?" (Just ["Evenly", "All on me", "All on them"])

done :: Reply
done = Reply "Done!" Nothing

apologizing :: Reply -> Reply
apologizing (Reply text options) =
  Reply ("Sorry, I couldn't understand that. " ++ text) options
