module Runtime (Runtime, initialize, start, onMessage) where

import qualified Conversation
import qualified Core
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Client.TLS (newTlsManager)
import qualified Queue
import           Queue (Queue)
import qualified Settings
import           Settings (Settings)
import qualified Splitwise
import qualified Telegram
import           Telegram.Message (Message)

data Runtime = Runtime
  { telegramToken :: Telegram.Token
  , splitwiseGroup :: Splitwise.Group
  , http :: Http.Manager
  , queue :: Queue Core.Event
  , core :: Core.Model
  }

initialize :: Settings -> IO Runtime
initialize settings = do
  queue       <- Queue.new
  httpManager <- newTlsManager
  return $ Runtime
    { telegramToken  = Telegram.Token $ Settings.telegramToken settings
    , splitwiseGroup = Splitwise.group (Settings.userASplitwiseToken settings)
                                       (Settings.userASplitwiseId settings)
                                       (Settings.userBSplitwiseId settings)
    , http           = httpManager
    , queue          = queue
    , core           = Core.initialize settings
    }

onMessage :: Runtime -> Message -> IO ()
onMessage runtime =
  \message -> Queue.enqueue (queue runtime) (Core.MessageReceived message)

start :: Runtime -> IO ()
start runtime = loop runtime

loop :: Runtime -> IO ()
loop runtime = do
  event <- Queue.dequeue (queue runtime)
  let (updatedCore, effects) = Core.update event (core runtime)
  runEffects runtime effects
  loop (runtime { core = updatedCore })

runEffects :: Runtime -> [Core.Effect] -> IO ()
runEffects runtime effects = case effects of
  [] -> return ()
  first : rest ->
    -- TODO: catch errors
    (runEffect runtime first) >> runEffects runtime rest

runEffect :: Runtime -> Core.Effect -> IO ()
runEffect runtime effect = case effect of
  Core.LogError msg -> putStrLn msg
  Core.ConversationEffect contactInfo splitwiseRole eff -> case eff of
    Conversation.Answer reply -> do
      -- TODO: log error if something went wrong
      _ <- Telegram.sendMessage (http runtime)
                                (telegramToken runtime)
                                (Core.chatId contactInfo)
                                reply
      return ()
    Conversation.Store expense -> do
      -- TODO: notify user via telegram if storing the expense didn't work
      _ <- Splitwise.createExpense (http runtime)
                                   splitwiseRole
                                   (splitwiseGroup runtime)
                                   expense
      return ()
    Conversation.ReportBalance reply -> do
      -- TODO: remove continuation from this effect's payload and replace it by
      -- another effect
      result <- Splitwise.getBalance (http runtime)
                                     (splitwiseGroup runtime)
                                     splitwiseRole

      _ <- Telegram.sendMessage (http runtime)
                                (telegramToken runtime)
                                (Core.chatId contactInfo)
                                (reply result)
      return ()
    Conversation.NotifyPeer reply -> do
      -- TODO: get balance once and handle all relevant notifications as
      -- different "send" effects inside Core.
      case (Core.peerChatId contactInfo) of
        Nothing ->
          -- this means that we don't know the peer's chat id because they
          -- haven't contacted us yet.
          return ()
        Just peerChatId -> do
          result <- Splitwise.getBalance (http runtime)
                                         (splitwiseGroup runtime)
                                         splitwiseRole
          _ <- Telegram.sendMessage (http runtime)
                                    (telegramToken runtime)
                                    peerChatId
                                    (reply result)
          return ()
