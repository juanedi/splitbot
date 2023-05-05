module Runtime (start) where

import Control.Concurrent.Async (concurrently)
import qualified Conversation
import qualified Core
import qualified LocalStore
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (newTlsManager)
import Queue (Queue)
import qualified Queue
import Settings (Settings)
import qualified Settings
import qualified Splitwise
import qualified Telegram
import qualified Telegram.Api
import qualified Telegram.LongPolling
import Telegram.Message (Message)
import qualified Telegram.Reply
import qualified Telegram.WebhookServer


data Runtime = Runtime
  { telegram :: Telegram.Handler
  , splitwise :: Splitwise.Handler
  , localStore :: LocalStore.Handler
  , queue :: Queue Core.Event
  , core :: Core.Model
  }


start :: Settings -> IO ()
start settings =
  case Settings.botMode settings of
    Settings.LongPolling -> do
      putStrLn "Starting bot in polling mode 🚀"
      startPolling settings
    Settings.Server port -> do
      putStrLn "Starting bot with webserver 🚀"
      startServer port settings


startPolling :: Settings -> IO ()
startPolling settings = do
  runtime <- Runtime.init settings
  _ <-
    concurrently
      (loop runtime)
      (Telegram.LongPolling.run (onMessage runtime) (telegram runtime))
  return ()


startServer :: Int -> Settings -> IO ()
startServer port settings = do
  runtime <- Runtime.init settings
  _ <-
    concurrently
      (loop runtime)
      ( Telegram.WebhookServer.run
          (Runtime.onMessage runtime)
          (Settings.telegramToken settings)
          port
      )
  return ()


init :: Settings -> IO Runtime
init settings = do
  let localStore = LocalStore.init (Settings.storePath settings)
  queue <- Queue.new
  httpManager <- newTlsManager
  core <- Core.initialize localStore settings
  let runtime =
        Runtime
          { telegram = Telegram.init httpManager (Telegram.Token $ Settings.telegramToken settings)
          , splitwise =
              Splitwise.init
                httpManager
                (Settings.userASplitwiseToken settings)
                (Settings.userASplitwiseId settings)
                (Settings.userBSplitwiseId settings)
          , localStore = localStore
          , queue = queue
          , core = core
          }
  return runtime


onMessage :: Runtime -> Message -> IO ()
onMessage runtime message =
  Queue.enqueue (queue runtime) (Core.MessageReceived message)


loop :: Runtime -> IO ()
loop runtime = do
  event <- Queue.dequeue (queue runtime)
  (updatedCore, effects) <- Core.update (localStore runtime) event (core runtime)
  runEffects runtime effects
  loop (runtime {core = updatedCore})


runEffects :: Runtime -> [Core.Effect] -> IO ()
runEffects runtime effects =
  case effects of
    [] -> return ()
    first : rest ->
      -- TODO: catch errors
      runEffect runtime first >> runEffects runtime rest


runEffect :: Runtime -> Core.Effect -> IO ()
runEffect runtime effect =
  case effect of
    Core.ConversationEffect contactInfo eff ->
      case eff of
        Conversation.Answer reply ->
          sendMessage runtime (Core.ownChatId contactInfo) reply
        Conversation.NotifyPeer reply -> case Core.peerChatId contactInfo of
          Nothing ->
            -- this means that we don't know the peer's chat id because they
            -- haven't contacted us yet.
            return ()
          Just peerChatId -> sendMessage runtime peerChatId reply
        Conversation.Store onOutcome expense -> do
          outcome <-
            Splitwise.createExpense
              (splitwise runtime)
              (Core.ownRole contactInfo)
              expense
          Queue.enqueue
            (queue runtime)
            ( Core.ConversationEvent (Core.ownUserId contactInfo) (onOutcome outcome)
            )
        Conversation.GetBalance onBalance -> do
          result <-
            Splitwise.getBalance
              (splitwise runtime)
              (Core.ownRole contactInfo)
          Queue.enqueue
            (queue runtime)
            (Core.ConversationEvent (Core.ownUserId contactInfo) (onBalance result))


sendMessage :: Runtime -> Telegram.Api.ChatId -> Telegram.Reply.Reply -> IO ()
sendMessage runtime chatId reply = do
  result <-
    Telegram.sendMessage
      (telegram runtime)
      chatId
      reply
  if result
    then return ()
    else -- TODO: retry once and only log after second failure
      putStrLn "ERROR! Could not send message via telegram API"
