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
import qualified Telegram
import           Telegram.Api (ChatId)
import           Telegram.Message (Message)
import qualified Telegram.Message as Message
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
    |> fmap (storeChatId (Session.chatId session))
    |> updateUser (Session.userId session) model
    |> Effectful.run (runEffect model session)

updateUser :: UserId -> Model -> Effectful Effect User -> Effectful Effect Model
updateUser userId model = fmap (Model.updateUser userId model)

storeChatId :: ChatId -> User -> User
storeChatId chatId user = user { Model.chatId = Just chatId }

reply :: User -> Message -> Effectful Effect User
reply user message = do
  updatedConversation <- case Model.conversation user of
    Nothing -> Conversation.start txt (Model.preset user)
    Just c  -> Conversation.advance txt c
  return user { Model.conversation = updatedConversation }
  where txt = (Message.text message)


runEffect :: Model -> Session -> Effect -> IO Bool
runEffect model session effect
  = let
      httpManager = Model.http model

      send        = Telegram.sendMessage httpManager
                                         (Model.telegramToken model)
                                         (Session.chatId session)

      notifyError = send
        (Reply.plain "Ooops, something went wrong! This might be a bug 🐛")

      splitwiseRole =
        Model.splitwiseRole ((Model.identity . Session.user) session)

      createExpense = Splitwise.createExpense httpManager
                                              splitwiseRole
                                              (Model.splitwiseAuth model)

      getBalance = Splitwise.getBalance httpManager
                                        (Model.splitwiseAuth model)
                                        splitwiseRole
    in
      case effect of
        Answer reply -> do
          send reply
        Store expense -> do
          success <- createExpense expense
          if success
            then return True
            else do
              _ <- notifyError
              return False
        ReportBalance reply -> do
          do
            result <- getBalance
            _      <- send (reply result)
            return True
