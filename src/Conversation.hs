module Conversation
  ( Conversation
  , Effect(..)
  , Expense(..)
  , Question(..)
  , advance
  , start
  ) where

import Conversation.Parameters
import Telegram (Reply(..), ReplyKeyboard(..))

data Conversation
  = AwaitingAmount
  | AwaitingPayer { amount :: Amount }
  | AwaitingSplit { payer :: Who
                  , amount :: Amount }

data Question
  = AskAmount
  | AskWhoPaid
  | AskHowToSplit

data Effect
  = Answer Reply
  | StoreAndReply Expense
                  Reply

data Expense = Expense
  { expensePayer :: Who
  , expenseAmount :: Amount
  , expenseSplit :: Split
  }

start :: (Maybe Conversation, [Effect])
start = (Just AwaitingAmount, [Answer askAmount])

advance :: String -> Conversation -> (Maybe Conversation, [Effect])
advance userMessage conversation =
  case conversation of
    AwaitingAmount ->
      case readAmount userMessage of
        Just amount ->
          (Just $ AwaitingPayer {amount = amount}, [Answer askWhoPaid])
        Nothing -> (Just conversation, [Answer $ apologizing askAmount])
    AwaitingPayer amount ->
      case readWho userMessage of
        Just payer ->
          ( Just $ AwaitingSplit {amount = amount, payer = payer}
          , [Answer askHowToSplit])
        Nothing -> (Just conversation, [Answer $ apologizing askWhoPaid])
    AwaitingSplit payer amount ->
      case readSplit userMessage of
        Just split ->
          ( Nothing
          , [ StoreAndReply
                (Expense
                   { expensePayer = payer
                   , expenseAmount = amount
                   , expenseSplit = split
                   })
                done
            ])
        Nothing -> (Just conversation, [Answer $ apologizing askHowToSplit])

askAmount :: Reply
askAmount = Reply "How much?" Normal

askWhoPaid :: Reply
askWhoPaid = Reply "Who paid?" (Options ["Me", "They"])

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
