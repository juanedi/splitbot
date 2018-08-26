module Conversation
  ( Conversation
  , Effect(..)
  , Expense(..)
  , Question(..)
  , advance
  , start
  ) where

import Conversation.Parameters
import qualified Conversation.Parameters.Amount as Amount
import qualified Conversation.Parameters.Payer as Payer
import qualified Conversation.Parameters.Split as Split
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
start preset = (Just (AwaitingAmount preset), [Answer Amount.ask])

advance :: String -> Conversation -> (Maybe Conversation, [Effect])
advance userMessage conversation =
  case conversation of
    AwaitingAmount preset ->
      case Amount.parse userMessage of
        Just amount ->
          ( Just $ AwaitingPayer {preset = preset, amount = amount}
          , [Answer Payer.ask])
        Nothing -> (Just conversation, [Answer $ apologizing Amount.ask])
    AwaitingPayer preset amount ->
      case Payer.parse userMessage of
        Just payer ->
          ( Just $
            AwaitingSplit {preset = preset, amount = amount, payer = payer}
          , [Answer (Split.ask preset)])
        Nothing -> (Just conversation, [Answer $ apologizing Payer.ask])
    AwaitingSplit preset payer amount ->
      case Split.parse userMessage of
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
          (Just conversation, [Answer $ apologizing (Split.ask preset)])

done :: Reply
done = Reply "Done!" Normal

apologizing :: Reply -> Reply
apologizing (Reply text options) =
  Reply ("Sorry, I couldn't understand that. " ++ text) options
