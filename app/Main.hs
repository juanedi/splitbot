module Main where

import Control.Arrow ((>>>))
import Conversation
import qualified Conversation.Replies as Replies
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Data.Traversable (sequence)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Migrations (migrateDB)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Settings
import Storage (createExpense)
import qualified Telegram

data State = State
  { currentConnection :: Connection
  , userA :: UserState
  , userB :: UserState
  , telegram :: Telegram.State
  }

data UserState = UserState
  { username :: Telegram.Username
  , buddy :: Telegram.Username
  , conversation :: Maybe Conversation
  }

data UserId
  = UserA
  | UserB
  deriving (Show)

main :: IO ()
main = do
  settings <- Settings.fromEnv
  manager <- newTlsManager
  conn <- connectPostgreSQL (pack (Settings.databaseUrl settings))
  let userA = (Telegram.Username . Settings.userA) settings
  let userB = (Telegram.Username . Settings.userB) settings
  migrateDB conn
  runServer $
    State
      { currentConnection = conn
      , userA = UserState userA userB Nothing
      , userB = UserState userB userA Nothing
      , telegram = Telegram.init (Settings.telegramToken settings) manager
      }

runServer :: State -> IO ()
runServer state = do
  (message, newState) <- getMessage state
  let username = Telegram.username message
  let maybeUserId = matchUserId state username
  case maybeUserId of
    Nothing -> do
      putStrLn $ "Ignoring message from unknown user" ++ show username
      runServer newState
    Just userId -> processMessage newState userId message >>= runServer

getMessage :: State -> IO (Telegram.Message, State)
getMessage state = do
  (message, newTelegramState) <- (telegram >>> Telegram.getMessage) (state)
  putStrLn $ "<< " ++ (Telegram.text message)
  return $ (message, state {telegram = newTelegramState})

matchUserId :: State -> Telegram.Username -> Maybe UserId
matchUserId state uname
  | (userA >>> username) state == uname = Just UserA
  | (userB >>> username) state == uname = Just UserB
  | otherwise = Nothing

processMessage :: State -> UserId -> Telegram.Message -> IO (State)
processMessage state userId message =
  let currentUserState = getUserState userId state
      (updatedConversation, effects) =
        (conversation >>>
         fmap (advance $ Telegram.text message) >>> fromMaybe start)
          currentUserState
      userState = currentUserState {conversation = updatedConversation}
      updatedState = setUserState userId userState state
   in do _ <- runEffects state userState (Telegram.chatId message) effects
         return updatedState

runEffects :: State -> UserState -> Telegram.ChatId -> [Effect] -> IO ()
runEffects state userState chatId effects = do
  _ <- sequence (run <$> effects)
  return ()
  where
    run = runEffect state userState chatId

runEffect :: State -> UserState -> Telegram.ChatId -> Effect -> IO ()
runEffect state userState chatId effect =
  let conn = currentConnection state
      telegramState = telegram state
   in case effect of
        Answer reply -> sendMessage telegramState chatId reply
        StoreAndReply expense reply -> do
          createExpense conn (username userState) (buddy userState) expense
          sendMessage telegramState chatId reply

sendMessage :: Telegram.State -> Telegram.ChatId -> Replies.Reply -> IO ()
sendMessage telegram chatId reply =
  let text = (Replies.text reply)
      options = (Replies.options reply)
   in do putStrLn $ ">> " ++ (Replies.text reply)
         Telegram.sendMessage telegram chatId text options

getUserState :: UserId -> State -> UserState
getUserState userId =
  case userId of
    UserA -> userA
    UserB -> userB

setUserState :: UserId -> UserState -> State -> State
setUserState userId newUserState state =
  case userId of
    UserA -> state {userA = newUserState}
    UserB -> state {userB = newUserState}
