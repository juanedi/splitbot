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
import           Data.Maybe (fromMaybe)
import           Splitwise.Api.Balance (Balance)
import qualified Splitwise.Api.Balance as Balance
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
  | Store Expense
  | ReportBalance (Maybe Balance -> Reply)
  | NotifyPeer (Maybe Balance -> Reply)

type Result = (Maybe Conversation, [Effect] )

start :: String -> Split -> Result
start message preset =
  let description = Description.read message
  in  case message of
        "/start" ->
          (Just (AwaitingDescription preset), [Answer (Description.ask)])

        _ ->
          ( Just (AwaitingInitialConfirmation preset description)
          , [Answer (Description.confirm description)]
          )

advance :: String -> Conversation -> Result
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
      ( Just
        (AwaitingPayer
          { preset      = preset
          , description = description
          , amount      = amount
          }
        )
      , [Answer Payer.ask]
      )
    Nothing -> (Just conversation, [Answer (Reply.apologizing Amount.ask)])

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

    Nothing -> (Just conversation, [Answer (Reply.apologizing Payer.ask)])

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
        in  ( Just (AwaitingConfirmation expense)
            , [Answer (Confirmation.ask expense)]
            )

      Nothing ->
        (Just conversation, [Answer (Reply.apologizing (Split.ask preset))])

  AwaitingConfirmation expense -> if Confirmation.read userMessage
    then
      ( Nothing
      , [ Answer holdOn
        , Store expense
        , ReportBalance done
        , NotifyPeer (peerNotification expense)
        ]
      )
    else (Nothing, [Answer cancelled])


holdOn :: Reply
holdOn = Reply.plain "Hold on a sec... ⏳"

done :: Maybe Balance -> Reply
done balanceResult = Reply.plain $ case balanceResult of
  Nothing      -> confirmation
  Just balance -> confirmation ++ balanceSummary balance
  where confirmation = "Done! 🎉 💸\n"

peerNotification :: Expense -> Maybe Balance -> Reply
peerNotification expense balanceResult = Reply.plain $ mconcat
  [ "Hey! A new expense was created! 💰"
  , "\n\n"
  , expenseSummary expense
  , "\n"
  , fromMaybe "" $ fmap (balanceSummary . Balance.invert) $ balanceResult
  ]


expenseSummary :: Expense -> String
expenseSummary expense = mconcat
  [ concat ["*", (Description.text . Expense.description) expense, "*\n"]
  , concat ["Total: $", show $ (Amount.value . Expense.amount) expense, "\n"]
  , concat
    ["Your share: ", show $ (Split.peerPart . Expense.split) expense, "%\n"]
  , "Payed by "
  , case (Expense.payer expense) of
    Me   -> "them"
    They -> "you"
  ]

balanceSummary :: Balance -> String
balanceSummary balance = case balance of
  []     -> "You're now even!"
  b : [] -> currencySummary b
  _      -> unlines $ currencySummary <$> balance
 where
  currencySummary cb = mconcat
    [ if Balance.amount cb >= 0 then "You are now owed" else "Now you owe"
    , " "
    , show (Balance.currency cb) ++ " " ++ show (abs $ Balance.amount cb)
    ]

cancelled :: Reply
cancelled = Reply.plain "Alright, the expense was discarded 👍"
