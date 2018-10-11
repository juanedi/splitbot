module Conversation
  ( Conversation
  , Effect(..)
  , Expense(..)
  , Question(..)
  , advance
  , start
  ) where

import           Conversation.Expense (Expense)
import qualified Conversation.Expense as Expense
import           Conversation.Parameters.Amount (Amount)
import qualified Conversation.Parameters.Amount as Amount
import qualified Conversation.Parameters.Confirmation as Confirmation
import           Conversation.Parameters.Description (Description)
import qualified Conversation.Parameters.Description as Description
import qualified Conversation.Parameters.Payer as Payer
import           Conversation.Parameters.Split (Split)
import qualified Conversation.Parameters.Split as Split
import           Conversation.Parameters.Who
import           Telegram.Reply (Reply)
import qualified Telegram.Reply as Reply

data Conversation
  = -- Initial state when user first contacts the bot via the '/start' command
    AwaitingDescription
    { preset :: Split
    }
  | -- Initial state when the user contacts the bot sending a description
    AwaitingInitialConfirmation
    { preset :: Split
    , description :: Description
    }
  | AwaitingAmount
    { preset :: Split
    , description :: Description
    }
  | AwaitingPayer
    { preset :: Split
    , description :: Description
    , amount :: Amount
    }
  | AwaitingSplit
    { preset :: Split
    , description :: Description
    , payer :: Who
    , amount :: Amount
    }
  | AwaitingConfirmation Expense

data Question
  = AskAmount
  | AskWhoPaid
  | AskHowToSplit

data Effect
  = Answer Reply
  | StoreAndReply Expense
                  Reply

start :: String -> Split -> (Maybe Conversation, [Effect])
start message preset = case message of
  "/start" -> (Just (AwaitingDescription preset), [Answer (Description.ask)])
  _ ->
    ( Just (AwaitingInitialConfirmation preset description)
    , [Answer (Description.confirm description)]
    )
    where description = Description.read message

advance :: String -> Conversation -> (Maybe Conversation, [Effect])
advance userMessage conversation = case conversation of
  AwaitingDescription preset ->
    ( Just $ AwaitingAmount
      { preset      = preset
      , description = Description.read userMessage
      }
    , [Answer Amount.ask]
    )

  AwaitingInitialConfirmation preset description ->
    if Description.readConfirmation userMessage
      then
        ( Just $ AwaitingAmount {preset = preset, description = description}
        , [Answer Amount.ask]
        )
      else (Nothing, [Answer cancelled])

  AwaitingAmount preset description -> case Amount.parse userMessage of
    Just amount ->
      ( Just $ AwaitingPayer
        { preset      = preset
        , description = description
        , amount      = amount
        }
      , [Answer Payer.ask]
      )
    Nothing -> (Just conversation, [Answer $ Reply.apologizing Amount.ask])

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
    Nothing -> (Just conversation, [Answer $ Reply.apologizing Payer.ask])

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
      Nothing ->
        (Just conversation, [Answer $ Reply.apologizing (Split.ask preset)])

  AwaitingConfirmation expense -> if Confirmation.read userMessage
    then (Nothing, [StoreAndReply expense done])
    else (Nothing, [Answer cancelled])

done :: Reply
done = Reply.plain "Done! 🎉 💸"

cancelled :: Reply
cancelled = Reply.plain "Alright, the expense was discarded 👍"
