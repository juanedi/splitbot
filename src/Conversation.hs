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
import qualified Telegram
import Telegram.Api (ChatId)
import Telegram.Reply (Reply)
import qualified Telegram.Reply as Reply


data Conversation
  = GatheringInfo Engine.State
  | SavingExpense Expense
  | FetchingBalance Expense


data Event
  = ExpenseCreationDone Splitwise.ExpenseOutcome
  | OnBalance (Maybe Balance)
  deriving (Show)


data Effect
  = NotifyPeer Reply
  | Store (Splitwise.ExpenseOutcome -> Event) Expense
  | GetBalance (Maybe Balance -> Event)


start ::
  Telegram.Handler ->
  ChatId ->
  String ->
  Split ->
  IO Conversation
start telegram chatId message preset =
  case Engine.start message preset of
    (engineState, reply) -> do
      Telegram.sendMessage telegram chatId reply
      pure (GatheringInfo engineState)


update ::
  Telegram.Handler ->
  ChatId ->
  Event ->
  Conversation ->
  IO (Maybe Conversation, [Effect])
update telegram chatId event conversation =
  case (conversation, event) of
    (SavingExpense expense, ExpenseCreationDone outcome) ->
      case outcome of
        Splitwise.Created ->
          pure (Just (FetchingBalance expense), [GetBalance OnBalance])
        Splitwise.Failed ->
          -- TODO: handle this error: maybe ask if we want to retry?
          pure (Just conversation, [])
    (FetchingBalance expense, OnBalance maybeBalance) -> do
      Telegram.sendMessage telegram chatId (ownNotification maybeBalance)
      pure
        ( Nothing
        , [NotifyPeer (peerNotification expense maybeBalance)]
        )
    (_, _) -> pure (Just conversation, [])


messageReceived ::
  Telegram.Handler ->
  ChatId ->
  String ->
  Conversation ->
  IO (Maybe Conversation, [Effect])
messageReceived telegram chatId userMessage conversation =
  case conversation of
    GatheringInfo engineState ->
      case Engine.update userMessage engineState of
        Engine.Continue engineState' reply -> do
          Telegram.sendMessage telegram chatId reply
          pure (Just (GatheringInfo engineState'), [])
        Engine.Terminate reply -> do
          Telegram.sendMessage telegram chatId reply
          pure (Nothing, [])
        Engine.Done expense -> do
          Telegram.sendMessage telegram chatId holdOn
          pure
            ( Just (SavingExpense expense)
            , [Store ExpenseCreationDone expense]
            )
    SavingExpense _ -> do
      Telegram.sendMessage telegram chatId holdOn
      pure (Just conversation, [])
    FetchingBalance _ -> do
      Telegram.sendMessage telegram chatId holdOn
      pure (Just conversation, [])


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
