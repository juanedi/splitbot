module Worker (run) where

import qualified Conversation
import           Conversation (Effect(..))
import qualified Effectful
import           Effectful (Effectful)
import           Network.HTTP.Client.TLS (newTlsManager)
import qualified Queue
import           Queue (Queue)
import           Settings (Settings)
import qualified Splitwise
import           Splitwise.Api.Balance (Balance)
import qualified Splitwise.Api.Balance as Balance
import qualified Telegram
import           Telegram.Message (Message)
import qualified Telegram.Message as Message
import           Telegram.Reply (Reply)
import qualified Telegram.Reply as Reply
import           Worker.Model (Model, User, UserId)
import qualified Worker.Model as Model
import           Worker.Session (Session)
import qualified Worker.Session as Session


(|>) :: a -> (a -> b) -> b
(|>) a f = f a

run :: Settings -> Queue Message -> IO ()
run settings queue = do
  httpManager <- newTlsManager
  loop queue (Model.initialize settings httpManager)

loop :: Queue Message -> Model -> IO ()
loop queue model = do
  msg <- Queue.dequeue queue
  case Session.load model msg of
    Nothing -> do
      putStrLn $ "Ignoring message from unknown user" ++ show
        (Message.username msg)
      return ()
    Just session -> do
      updatedState <- processMessage model session msg
      loop queue updatedState

processMessage :: Model -> Session -> Message -> IO Model
processMessage model session message =
  message
    |> reply (Session.user session)
    |> updateUser (Session.userId session) model
    |> Effectful.run (runEffect model session)

updateUser :: UserId -> Model -> Effectful Effect User -> Effectful Effect Model
updateUser userId model = fmap (Model.updateUser userId model)

reply :: User -> Message -> Effectful Effect User
reply user message = do
  updatedConversation <- case Model.conversation user of
    Nothing -> Conversation.start txt (Model.preset user)
    Just c  -> Conversation.advance txt c
  return user { Model.conversation = updatedConversation }
  where txt = (Message.text message)


runEffect :: Model -> Session -> Effect -> IO Bool
runEffect model session effect =
  let httpManager = Model.http model

      send        = Telegram.sendMessage httpManager
                                         (Model.telegramToken model)
                                         (Session.chatId session)

      notifyError = send
        (Reply.plain "Ooops, something went wrong! This might be a bug 🐛")

      createExpense = Splitwise.createExpense
        httpManager
        (Model.splitwiseId ((Model.identity . Session.user) session))
        (Model.splitwiseId (Session.buddy session))
        (Model.splitwiseToken model)

      getBalance = Splitwise.getBalance
        httpManager
        (Model.splitwiseToken model)
        (Model.splitwiseId (Session.buddy session))
  in  case effect of
        Answer reply -> do
          send reply
        Store expense -> do
          success <- createExpense expense
          if success
            then do
              balanceResult <- getBalance
              case balanceResult of
                Nothing      -> return True
                Just balance -> do
                  _ <- send (balanceSummary balance)
                  return True
            else do
              _ <- notifyError
              return False

balanceSummary :: Balance -> Reply
balanceSummary balance = case balance of
  []     -> Reply.plain "You're even!"
  b : [] -> Reply.plain $ currencySummary b
  _      -> Reply.plain $ unlines $ currencySummary <$> balance
 where
  currencySummary cb = prefix cb ++ " " ++ value cb
  prefix cb = if Balance.amount cb >= 0 then "You are owed" else "You owe"
  value cb =
    show (Balance.currency cb) ++ " " ++ show (abs $ Balance.amount cb)
