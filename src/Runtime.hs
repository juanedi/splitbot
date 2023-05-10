module Runtime (start) where

import Control.Applicative (liftA2)
import Control.Concurrent.Async (concurrently)
import qualified Conversation
import qualified Core
import Data.Maybe (maybe)
import qualified LocalStore
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (newTlsManager)
import qualified OpenAI
import Queue (Queue)
import qualified Queue
import Settings (Settings)
import qualified Settings
import qualified Splitwise
import qualified Telegram
import qualified Telegram.LongPolling
import Telegram.Message (Message)
import qualified Telegram.WebhookServer


data Runtime = Runtime
  { telegram :: Telegram.Handler
  , splitwise :: Splitwise.Handler
  , localStore :: LocalStore.Handler
  , queue :: Queue Message
  , engine :: Conversation.Engine
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
          (onMessage runtime)
          (Settings.telegramToken settings)
          port
      )
  return ()


init :: Settings -> IO Runtime
init settings = do
  let localStore = LocalStore.init (Settings.storePath settings)
  queue <- Queue.new
  httpManager <- newTlsManager
  core <- Core.init localStore settings
  let engine =
        case (Settings.openAIToken settings, Settings.openAIPromptTemplate settings) of
          (Just token, Just promptTemplate) ->
            Conversation.GPT (OpenAI.init httpManager token) promptTemplate
          _ ->
            Conversation.Basic
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
          , engine = engine
          , core = core
          }
  return runtime


onMessage :: Runtime -> Message -> IO ()
onMessage runtime =
  Queue.enqueue (queue runtime)


loop :: Runtime -> IO ()
loop runtime = do
  event <- Queue.dequeue (queue runtime)
  updatedCore <-
    Core.update
      (engine runtime)
      (telegram runtime)
      (splitwise runtime)
      (localStore runtime)
      event
      (core runtime)
  loop (runtime {core = updatedCore})
