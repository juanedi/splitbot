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
import qualified Telegram.Reply as Reply

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
  runEffects effects
  loop (runtime { core = updatedCore })

runEffects :: [Core.Effect] -> IO ()
runEffects effects = case effects of
  [] -> return ()
  first : rest ->
    -- TODO: catch errors
    (runEffect first) >> runEffects rest

runEffect :: Core.Effect -> IO ()
runEffect effect = case effect of
  Core.LogError           msg -> putStrLn msg
  Core.ConversationEffect eff -> case eff of
    Conversation.Answer reply ->
      putStrLn ("asked to answer: " ++ Reply.text reply)
    Conversation.Store         _ -> putStrLn "asked to store expense!"
    Conversation.ReportBalance _ -> putStrLn "asked to report balance!"
    Conversation.NotifyPeer    _ -> putStrLn "asked to notify peer!"
