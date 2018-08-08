module Conversation.Replies where

askAmount :: String
askAmount = "How much?"

askWhoPaid :: String
askWhoPaid = "Who paid?"

askHowToSplit :: String
askHowToSplit = "How will you split it?"

apologizing :: String -> String
apologizing message = "Sorry, I couldn't understand that. " ++ message

done :: String
done = "Done!"
