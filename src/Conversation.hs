module Conversation (
  Conversation,
  Expense (..),
  messageReceived,
  Conversation.start,
) where

import Conversation.BasicEngine as BasicEngine
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


newtype Conversation
  = Conversation BasicEngine.State


start ::
  Telegram.Handler ->
  ChatId ->
  String ->
  Split ->
  IO Conversation
start telegram chatId message preset = do
  (engineState, reply) <- BasicEngine.start message preset
  Telegram.sendMessage telegram chatId reply
  pure (Conversation engineState)


messageReceived ::
  Telegram.Handler ->
  Splitwise.Handler ->
  ChatId ->
  Maybe ChatId ->
  Splitwise.Role ->
  String ->
  Conversation ->
  IO (Maybe Conversation)
messageReceived telegram splitwise chatId maybePeerChatId ownRole userMessage (Conversation engineState) = do
  outcome <- BasicEngine.update userMessage engineState
  case outcome of
    BasicEngine.Continue engineState' reply -> do
      Telegram.sendMessage telegram chatId reply
      pure (Just (Conversation engineState'))
    BasicEngine.Terminate reply -> do
      Telegram.sendMessage telegram chatId reply
      pure Nothing
    BasicEngine.Done expense -> do
      Telegram.sendMessage telegram chatId holdOn
      outcome <- Splitwise.createExpense splitwise ownRole expense
      case outcome of
        Splitwise.Created -> do
          maybeBalance <- Splitwise.getBalance splitwise ownRole
          Telegram.sendMessage telegram chatId (ownNotification maybeBalance)
          notifyPeer
            telegram
            maybePeerChatId
            (peerNotification expense maybeBalance)

          pure Nothing
        Splitwise.Failed ->
          -- TODO: handle this error: maybe ask if we want to retry?
          pure (Just (Conversation engineState))


notifyPeer :: Telegram.Handler -> Maybe ChatId -> Reply -> IO ()
notifyPeer telegram maybePeerChatId msg =
  case maybePeerChatId of
    Nothing ->
      pure ()
    Just peerChatId ->
      Telegram.sendMessage telegram peerChatId msg


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
