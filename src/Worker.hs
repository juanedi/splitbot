module Worker (run) where

import qualified Conversation
import           Conversation (Effect(..))
import           Network.HTTP.Client.TLS (newTlsManager)
import qualified Queue
import           Queue (Queue)
import           Settings (Settings)
import qualified Splitwise
import qualified Telegram
import           Telegram.Message (Message)
import qualified Telegram.Message as Message
import qualified Telegram.Reply as Reply
import           Worker.Model (Model, User)
import qualified Worker.Model as Model
import           Worker.Session (Session)
import qualified Worker.Session as Session

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
  let (user', effects) = reply (Session.user session) message
      model' = Model.updateUser (Session.userId session) (const user') model
  in  do
        runEffects model' (session { Session.user = user' }) effects
        return model'

reply :: User -> Message -> (User, [Effect])
reply user message =
  let messageText                    = Message.text message
      (updatedConversation, effects) = case Model.conversation user of
        Nothing -> Conversation.start messageText (Model.preset user)
        Just c  -> Conversation.advance messageText c
  in  (user { Model.conversation = updatedConversation }, effects)


runEffects :: Model -> Session -> [Effect] -> IO ()
runEffects model session effects = do
  _ <- sequence (run <$> effects)
  return ()
  where run = runEffect model session

runEffect :: Model -> Session -> Effect -> IO ()
runEffect model session effect =
  let httpManager = Model.http model
  in  case effect of
        Answer reply -> Telegram.sendMessage httpManager
                                             (Model.telegramToken model)
                                             (Session.chatId session)
                                             reply
        Done expense -> do
          sucess <- Splitwise.createExpense
            httpManager
            (Model.splitwiseId ((Model.identity . Session.user) session))
            (Model.splitwiseId (Session.buddy session))
            expense
            (Model.splitwiseToken model)
          if sucess
            then Telegram.sendMessage httpManager
                                      (Model.telegramToken model)
                                      (Session.chatId session)
                                      (Reply.plain "Done! 🎉 💸")
            else return ()
