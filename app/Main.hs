module Main where

import Control.Arrow ((>>>))
import Conversation
import Conversation.Parameters (Split(..))
import Data.ByteString.Char8 (pack)
import Data.Traversable (sequence)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Settings
import qualified Storage
import Telegram.Api (ChatId, Username(..), Reply(..))
import Telegram (Message)
import qualified Telegram

data State = State
  { currentConnection :: Connection
  , http :: Http.Manager
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
  settings    <- Settings.fromEnv
  httpManager <- newTlsManager
  conn        <- connectPostgreSQL (pack (Settings.databaseUrl settings))
  let userA   = (Username . Settings.userA) settings
      userB   = (Username . Settings.userB) settings
      presetA = Settings.presetA settings
      presetB = 100 - presetA
  Storage.migrateDB conn
  runServer $ State
    { currentConnection = conn
    , http              = httpManager
    , userA             = UserState userA userB Nothing (Split presetA)
    , userB             = UserState userB userA Nothing (Split presetB)
    , telegram          = Telegram.init (Settings.telegramToken settings)
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
  let manager = http state
  (message, newTelegramState) <- (telegram >>> Telegram.getMessage manager)
    state
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
      httpManager   = http state
      telegramState = telegram state
  in  case effect of
        Answer reply -> sendMessage httpManager telegramState chatId reply
        StoreAndReply expense reply -> do
          Storage.createExpense conn
                                (username userState)
                                (buddy userState)
                                expense
          sendMessage httpManager telegramState chatId reply

sendMessage :: Http.Manager -> Telegram.State -> ChatId -> Reply -> IO ()
sendMessage http telegram chatId reply@(Reply text _) = do
  putStrLn $ ">> " ++ text
  Telegram.sendMessage http telegram chatId reply

getUserState :: UserId -> State -> UserState
getUserState userId = case userId of
  UserA -> userA
  UserB -> userB

setUserState :: UserId -> UserState -> State -> State
setUserState userId newUserState state = case userId of
  UserA -> state { userA = newUserState }
  UserB -> state { userB = newUserState }
