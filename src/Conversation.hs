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
  = AwaitingAmount { preset :: Split }
  | AwaitingPayer { preset :: Split
                  , amount :: Amount }
  | AwaitingSplit { preset :: Split
                  , payer :: Who
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

start :: Split -> (Maybe Conversation, [Effect])
start preset = (Just (AwaitingAmount preset), [Answer askAmount])

advance :: String -> Conversation -> (Maybe Conversation, [Effect])
advance userMessage conversation =
  case conversation of
    AwaitingAmount preset ->
      case readAmount userMessage of
        Just amount ->
          ( Just $ AwaitingPayer {preset = preset, amount = amount}
          , [Answer askWhoPaid])
        Nothing -> (Just conversation, [Answer $ apologizing askAmount])
    AwaitingPayer preset amount ->
      case readWho userMessage of
        Just payer ->
          ( Just $
            AwaitingSplit {preset = preset, amount = amount, payer = payer}
          , [Answer (askHowToSplit preset)])
        Nothing -> (Just conversation, [Answer $ apologizing askWhoPaid])
    AwaitingSplit preset payer amount ->
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
        Nothing ->
          (Just conversation, [Answer $ apologizing (askHowToSplit preset)])

askAmount :: Reply
askAmount = Reply "How much?" Normal

askWhoPaid :: Reply
askWhoPaid = Reply "Who paid?" (Options ["Me", "They"])

askHowToSplit :: Split -> Reply
askHowToSplit preset =
  Reply
    "How will you split it?"
    (Options
       [ "Evenly"
       , "All on me"
       , "All on them"
       , "I paid " ++ show (myPart preset) ++ "%"
       ])

done :: Reply
done = Reply "Done!" Normal

apologizing :: Reply -> Reply
apologizing (Reply text options) =
  Reply ("Sorry, I couldn't understand that. " ++ text) options
