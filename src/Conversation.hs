module Conversation (
  Conversation,
  Event,
  Effect (..),
  Expense (..),
  messageReceived,
  Conversation.start,
  Conversation.update,
) where

import Conversation.Engine as Engine
import Conversation.Expense (Expense, Split, Who (..))
import qualified Conversation.Expense as Expense
import Data.Maybe (fromMaybe)
import qualified Splitwise
import Splitwise.Api.Balance (Balance)
import qualified Splitwise.Api.Balance as Balance
import Telegram.Reply (Reply)
import qualified Telegram.Reply as Reply


data Conversation
  = GatheringInfo Engine.State
  | AwaitingConfirmation Expense
  | SavingExpense Expense
  | FetchingBalance Expense


data Event
  = ExpenseCreationDone Splitwise.ExpenseOutcome
  | OnBalance (Maybe Balance)
  deriving (Show)


data Effect
  = Answer Reply
  | NotifyPeer Reply
  | Store (Splitwise.ExpenseOutcome -> Event) Expense
  | GetBalance (Maybe Balance -> Event)


start :: String -> Split -> (Maybe Conversation, [Effect])
start message preset =
  case Engine.start message preset of
    (engineState, reply) ->
      (Just (GatheringInfo engineState), [Answer reply])


update :: Event -> Conversation -> (Maybe Conversation, [Effect])
update event conversation =
  case (conversation, event) of
    (SavingExpense expense, ExpenseCreationDone outcome) ->
      case outcome of
        Splitwise.Created ->
          (Just (FetchingBalance expense), [GetBalance OnBalance])
        Splitwise.Failed ->
          -- TODO: handle this error: maybe ask if we want to retry?
          (Just conversation, [])
    (FetchingBalance expense, OnBalance maybeBalance) ->
      ( Nothing
      ,
        [ Answer (ownNotification maybeBalance)
        , NotifyPeer (peerNotification expense maybeBalance)
        ]
      )
    (_, _) -> (Just conversation, [])


messageReceived :: String -> Conversation -> (Maybe Conversation, [Effect])
messageReceived userMessage conversation =
  case conversation of
    GatheringInfo engineState ->
      case Engine.update userMessage engineState of
        Engine.Continue engineState' reply ->
          ( Just (GatheringInfo engineState')
          , [Answer reply]
          )
        Engine.Terminate reply ->
          ( Nothing
          , [Answer reply]
          )
        Engine.Confirm expense ->
          ( Just (AwaitingConfirmation expense)
          , [Answer (askForConfirmation expense)]
          )
    AwaitingConfirmation expense ->
      if userMessage == "Yes"
        then
          ( Just (SavingExpense expense)
          , [Answer holdOn, Store ExpenseCreationDone expense]
          )
        else (Nothing, [Answer cancelled])
    SavingExpense _ -> (Just conversation, [Answer holdOn])
    FetchingBalance _ -> (Just conversation, [Answer holdOn])


holdOn :: Reply
holdOn = Reply.plain "Hold on a sec... ⏳"


ownNotification :: Maybe Balance -> Reply
ownNotification balanceResult =
  Reply.plain
    ( case balanceResult of
        Nothing -> confirmation
        Just balance -> confirmation ++ balanceSummary balance
    )
  where
    confirmation = "Done! 🎉 💸\n"


peerNotification :: Expense -> Maybe Balance -> Reply
peerNotification expense balanceResult =
  Reply.plain $
    mconcat
      [ "Hey! A new expense was created! 💰"
      , "\n\n"
      , expenseSummary expense
      , "\n"
      , maybe "" (balanceSummary . Balance.invert) balanceResult
      ]


expenseSummary :: Expense -> String
expenseSummary expense =
  mconcat
    [ concat ["*", (Expense.descriptionText . Expense.description) expense, "*\n"]
    , concat ["Total: $", show $ (Expense.amountValue . Expense.amount) expense, "\n"]
    , concat
        ["Your share: ", show $ (Expense.peerPart . Expense.split) expense, "%\n"]
    , "Payed by "
    , case Expense.payer expense of
        Me -> "them"
        They -> "you"
    ]


balanceSummary :: Balance -> String
balanceSummary balance =
  case balance of
    [] ->
      "You're now even!"
    [b] ->
      currencySummary b
    _ ->
      unlines $ currencySummary <$> balance
  where
    currencySummary cb =
      mconcat
        [ if Balance.amount cb >= 0 then "You are now owed" else "Now you owe"
        , " "
        , show (Balance.currency cb) ++ " " ++ show (abs $ Balance.amount cb)
        ]


askForConfirmation :: Expense -> Reply
askForConfirmation expense =
  let descriptionLine =
        concat ["*", (Expense.descriptionText . Expense.description) expense, "*\n"]

      payerLine =
        concat
          [ "Payed by "
          , case Expense.payer expense of
              Me -> "me"
              They -> "them"
          , "\n"
          ]

      amountLine =
        concat ["Total: $", show $ (Expense.amountValue . Expense.amount) expense, "\n"]

      splitLine =
        concat ["I owe ", show $ Expense.myPart (Expense.split expense), "%", "\n"]
   in Reply.withOptions
        ( concat
            [ "Is this correct❓\n\n"
            , descriptionLine
            , amountLine
            , payerLine
            , splitLine
            ]
        )
        ["Yes", "No"]


cancelled :: Reply
cancelled = Reply.plain "Alright, the expense was discarded 👍"
