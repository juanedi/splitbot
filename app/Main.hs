module Main where

import Control.Arrow ((>>>))
import Conversation
import Conversation.Parameters (Split(..))
import Data.ByteString.Char8 (pack)
import Data.Traversable (sequence)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Settings
import qualified Storage
import Telegram (ChatId, Message, Username(..))
import qualified Telegram

data State = State
  { currentConnection :: Connection
  , userA :: UserState
  , userB :: UserState
  , telegram :: Telegram.State
  }

data UserState = UserState
  { username :: Username
  , buddy :: Username
  , conversation :: Maybe Conversation
  , preset :: Split
  }

data UserId
  = UserA
  | UserB
  deriving (Show)

main :: IO ()
main = do
  settings <- Settings.fromEnv
  manager  <- newTlsManager
  conn     <- connectPostgreSQL (pack (Settings.databaseUrl settings))
  let userA   = (Username . Settings.userA) settings
  let userB   = (Username . Settings.userB) settings
  let presetA = Settings.presetA settings
  let presetB = 100 - presetA
  Storage.migrateDB conn
  runServer $ State
    { currentConnection = conn
    , userA             = UserState userA userB Nothing (Split presetA)
    , userB             = UserState userB userA Nothing (Split presetB)
    , telegram = Telegram.init (Settings.telegramToken settings) manager
    }

runServer :: State -> IO ()
runServer state = do
  (message, newState) <- getMessage state
  let username    = Telegram.username message
  let maybeUserId = matchUserId state username
  case maybeUserId of
    Nothing -> do
      putStrLn $ "Ignoring message from unknown user" ++ show username
      runServer newState
    Just userId -> processMessage newState userId message >>= runServer

getMessage :: State -> IO (Message, State)
getMessage state = do
  (message, newTelegramState) <- (telegram >>> Telegram.getMessage) (state)
  putStrLn $ "<< " ++ (Telegram.text message)
  return $ (message, state { telegram = newTelegramState })

matchUserId :: State -> Username -> Maybe UserId
matchUserId state uname | (userA >>> username) state == uname = Just UserA
                        | (userB >>> username) state == uname = Just UserB
                        | otherwise                           = Nothing

processMessage :: State -> UserId -> Message -> IO (State)
processMessage state userId message =
  let currentUserState               = getUserState userId state
      currentConversation            = conversation currentUserState
      (updatedConversation, effects) = case currentConversation of
        Nothing -> start (preset currentUserState)
        Just c  -> advance (Telegram.text message) c
      userState    = currentUserState { conversation = updatedConversation }
      updatedState = setUserState userId userState state
  in  do
        _ <- runEffects state userState (Telegram.chatId message) effects
        return updatedState

runEffects :: State -> UserState -> ChatId -> [Effect] -> IO ()
runEffects state userState chatId effects = do
  _ <- sequence (run <$> effects)
  return ()
  where run = runEffect state userState chatId

runEffect :: State -> UserState -> ChatId -> Effect -> IO ()
runEffect state userState chatId effect =
  let conn          = currentConnection state
      telegramState = telegram state
  in  case effect of
        Answer reply                -> sendMessage telegramState chatId reply
        StoreAndReply expense reply -> do
          Storage.createExpense conn
                                (username userState)
                                (buddy userState)
                                expense
          sendMessage telegramState chatId reply

sendMessage :: Telegram.State -> ChatId -> Telegram.Reply -> IO ()
sendMessage telegram chatId reply@(Telegram.Reply text _) = do
  putStrLn $ ">> " ++ text
  Telegram.sendMessage telegram chatId reply

getUserState :: UserId -> State -> UserState
getUserState userId = case userId of
  UserA -> userA
  UserB -> userB

setUserState :: UserId -> UserState -> State -> State
setUserState userId newUserState state = case userId of
  UserA -> state { userA = newUserState }
  UserB -> state { userB = newUserState }
