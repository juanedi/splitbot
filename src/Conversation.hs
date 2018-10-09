module Conversation
  ( Conversation
  , Effect(..)
  , Expense(..)
  , Question(..)
  , advance
  , start
  ) where

import Conversation.Expense (Expense)
import qualified Conversation.Expense as Expense
import Conversation.Parameters
import qualified Conversation.Parameters.Amount as Amount
import qualified Conversation.Parameters.Confirmation as Confirmation
import qualified Conversation.Parameters.Description as Description
import qualified Conversation.Parameters.Payer as Payer
import qualified Conversation.Parameters.Split as Split
import Telegram.Api (Reply(..), ReplyKeyboard(..))

data Conversation
  = AwaitingDescription { preset :: Split }
  | AwaitingAmount { preset :: Split
                   , description :: Description
                   }
  | AwaitingPayer { preset :: Split
                  , description :: Description
                  , amount :: Amount }
  | AwaitingSplit { preset :: Split
                  , description :: Description
                  , payer :: Who
                  , amount :: Amount }
  | AwaitingConfirmation Expense

data Question
  = AskAmount
  | AskWhoPaid
  | AskHowToSplit

data Effect
  = Answer Reply
  | StoreAndReply Expense
                  Reply

start :: Split -> (Maybe Conversation, [Effect])
start preset = (Just (AwaitingDescription preset), [Answer Description.ask])

advance :: String -> Conversation -> (Maybe Conversation, [Effect])
advance userMessage conversation = case conversation of
  AwaitingDescription preset ->
    ( Just $ AwaitingAmount
      { preset      = preset
      , description = Description.read userMessage
      }
    , [Answer Amount.ask]
    )

  AwaitingAmount preset description -> case Amount.parse userMessage of
    Just amount ->
      ( Just $ AwaitingPayer
        { preset      = preset
        , description = description
        , amount      = amount
        }
      , [Answer Payer.ask]
      )
    Nothing -> (Just conversation, [Answer $ apologizing Amount.ask])

  AwaitingPayer preset description amount -> case Payer.parse userMessage of
    Just payer ->
      ( Just $ AwaitingSplit
        { preset      = preset
        , description = description
        , amount      = amount
        , payer       = payer
        }
      , [Answer (Split.ask preset)]
      )
    Nothing -> (Just conversation, [Answer $ apologizing Payer.ask])

  AwaitingSplit preset description payer amount ->
    case Split.parse userMessage of
      Just split ->
        let expense =
              (Expense.Expense
                { Expense.description = description
                , Expense.payer       = payer
                , Expense.amount      = amount
                , Expense.split       = split
                }
              )
        in  ( Just $ AwaitingConfirmation expense
            , [Answer (Confirmation.ask expense)]
            )
      Nothing -> (Just conversation, [Answer $ apologizing (Split.ask preset)])

  AwaitingConfirmation expense -> case Confirmation.read userMessage of
    True  -> (Nothing, [StoreAndReply expense done])
    False -> (Nothing, [Answer cancelled])

done :: Reply
done = Reply "Done! 🎉 💸" Normal

cancelled :: Reply
cancelled = Reply "Alright, the expense was discarded 👍" Normal

apologizing :: Reply -> Reply
apologizing (Reply text options) =
  Reply ("Sorry, I couldn't understand that. " ++ text) options
