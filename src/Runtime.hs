module Runtime (startPolling, startServer) where

import           Control.Concurrent.Async (concurrently)
import qualified Control.Exception
import qualified Conversation
import qualified Core
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Client.TLS (newTlsManager)
import qualified Queue
import           Queue (Queue)
import qualified Settings
import           Settings (Settings)
import qualified Splitwise
import qualified System.Directory
import qualified System.FilePath.Posix as FilePath
import qualified Telegram
import qualified Telegram.Api
import qualified Telegram.LongPolling
import           Telegram.Message (Message)
import qualified Telegram.Reply
import qualified Telegram.WebhookServer
import           Text.Read (readMaybe)

data Runtime = Runtime
  { telegramToken :: Telegram.Token
  , splitwiseGroup :: Splitwise.Group
  , storePath :: FilePath
  , http :: Http.Manager
  , queue :: Queue Core.Event
  , core :: Core.Model
  }

startPolling :: IO ()
startPolling = do
  settings <- Settings.fromEnv
  runtime  <- Runtime.init settings
  _        <- concurrently
    (loop runtime)
    (Telegram.LongPolling.run (onMessage runtime) (telegramToken runtime))
  return ()

startServer :: IO ()
startServer = do
  settings <- Settings.fromEnv
  runtime  <- Runtime.init settings
  _        <- concurrently
    (loop runtime)
    (Telegram.WebhookServer.run (Runtime.onMessage runtime)
                                (Settings.telegramToken settings)
                                (Settings.port settings)
    )
  return ()


init :: Settings -> IO Runtime
init settings = do
  queue       <- Queue.new
  httpManager <- newTlsManager
  let (core, initialEffects) = Core.initialize settings
      runtime                = Runtime
        { telegramToken  = Telegram.Token $ Settings.telegramToken settings
        , splitwiseGroup = Splitwise.group
          (Settings.userASplitwiseToken settings)
          (Settings.userASplitwiseId settings)
          (Settings.userBSplitwiseId settings)
        , storePath = Settings.storePath settings
        , http           = httpManager
        , queue          = queue
        , core           = core
        }
  runEffects runtime initialEffects
  return runtime

onMessage :: Runtime -> Message -> IO ()
onMessage runtime =
  \message -> Queue.enqueue (queue runtime) (Core.MessageReceived message)

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
  Core.LogError   msg    -> putStrLn msg

  Core.LoadChatId userId -> do
    maybeChatId <- readChatId (storePath runtime) userId
    case maybeChatId of
      Nothing     -> return ()
      Just chatId -> do
        Queue.enqueue (queue runtime) (Core.ChatIdLoaded userId chatId)

  Core.PersistChatId      userId      chatId ->
    persistChatId (storePath runtime) userId chatId

  Core.ConversationEffect contactInfo eff    -> case eff of
    Conversation.Answer reply ->
      sendMessage runtime (Core.ownChatId contactInfo) reply

    Conversation.NotifyPeer reply -> case Core.peerChatId contactInfo of
      Nothing ->
          -- this means that we don't know the peer's chat id because they
          -- haven't contacted us yet.
        return ()

      Just peerChatId -> sendMessage runtime peerChatId reply

    Conversation.Store onOutcome expense -> do
      outcome <- Splitwise.createExpense (http runtime)
                                         (Core.ownRole contactInfo)
                                         (splitwiseGroup runtime)
                                         expense
      Queue.enqueue
        (queue runtime)
        (Core.ConversationEvent (Core.ownUserId contactInfo) (onOutcome outcome)
        )

    Conversation.GetBalance onBalance -> do
      result <- Splitwise.getBalance (http runtime)
                                     (splitwiseGroup runtime)
                                     (Core.ownRole contactInfo)
      Queue.enqueue
        (queue runtime)
        (Core.ConversationEvent (Core.ownUserId contactInfo) (onBalance result))

sendMessage :: Runtime -> Telegram.Api.ChatId -> Telegram.Reply.Reply -> IO ()
sendMessage runtime chatId reply = do
  result <- Telegram.sendMessage (http runtime)
                                 (telegramToken runtime)
                                 chatId
                                 reply
  if result
    then return ()
    else
    -- TODO: retry once and only log after second failure
         putStrLn "ERROR! Could not send message via telegram API"

persistChatId :: FilePath -> Core.UserId -> Telegram.Api.ChatId -> IO ()
persistChatId storePath userId (Telegram.Api.ChatId chatId) =
  let (directoryPath, filePath) = chatIdPath storePath userId
  in  do
        System.Directory.createDirectoryIfMissing True directoryPath
        writeFile filePath (show chatId)

readChatId :: FilePath -> Core.UserId -> IO (Maybe (Telegram.Api.ChatId))
readChatId storePath userId =
  let (_, filePath) = chatIdPath storePath userId
  in  do
        readResult <- tryReadFile filePath
        return $ case readResult of
          Left  _err     -> Nothing
          Right contents -> fmap Telegram.Api.ChatId (readMaybe contents)

tryReadFile :: FilePath -> IO (Either IOError String)
tryReadFile path = Control.Exception.try (readFile path)

chatIdPath :: FilePath -> Core.UserId -> (FilePath, FilePath)
chatIdPath storePath userId =
  let userIdPart = case userId of
        Core.UserA -> "a"
        Core.UserB -> "b"
      directoryPath = FilePath.joinPath [storePath, "users", userIdPart]
  in  (directoryPath, FilePath.joinPath [directoryPath, "chat_id"])
