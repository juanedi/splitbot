module Main where

import Control.Arrow ((>>>))
import Conversation
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Data.Traversable (sequence)
import Database.PostgreSQL.Simple
import Migrations (migrateDB)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Environment (getEnv)
import qualified Telegram

data State = State
  { currentConnection :: Connection
  , userA :: UserState
  , userB :: UserState
  , telegram :: Telegram.State
  }

data UserId
  = UserA
  | UserB
  deriving (Show)

data UserState = UserState
  { username :: Telegram.Username
  , buddy :: Telegram.Username
  , conversation :: Maybe Conversation
  }

main :: IO ()
main = do
  userA <- getEnv "USER_A"
  userB <- getEnv "USER_B"
  url <- getEnv "DB_URL"
  telegramToken <- getEnv "TELEGRAM_TOKEN"
  manager <- newTlsManager
  conn <- connectPostgreSQL (pack url)
  migrateDB conn
  runServer $
    State
      { currentConnection = conn
      , userA = UserState (Telegram.Username userA) (Telegram.Username userB) Nothing
      , userB = UserState (Telegram.Username userB) (Telegram.Username userA) Nothing
      , telegram = Telegram.init telegramToken manager
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
        (conversation >>> fmap (advance $ Telegram.text message) >>> fromMaybe start)
          currentUserState
      userState = currentUserState {conversation = updatedConversation}
      updatedState = setUserState userId userState state
   in do _ <- runEffects state userState (Telegram.chatId message) effects
         return updatedState

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
        Ask question -> sendMessage telegramState chatId (questionText question)
        ApologizeAndAsk question ->
          sendMessage telegramState chatId (apologizing $ questionText question)
        StoreAndConfirm expense -> do
          createExpense conn (username userState) (buddy userState) expense
          sendMessage telegramState chatId "Done!"

sendMessage :: Telegram.State -> Telegram.ChatId -> String -> IO ()
sendMessage telegram chatId text = do
  putStrLn $ ">> " ++ text
  Telegram.sendMessage telegram chatId text

apologizing :: String -> String
apologizing message = "Sorry, I couldn't understand that. " ++ message

questionText :: Question -> String
questionText question =
  case question of
    AskAmount -> "How much?"
    AskWhoPaid -> "Who paid?"
    AskHowToSplit -> "How will you split it?"

createExpense :: Connection -> Telegram.Username -> Telegram.Username -> Expense -> IO ()
createExpense conn currentUser otherUser expense =
  let split = expenseSplit expense
      (payer, buddy) =
        case expensePayer expense of
          Me -> (currentUser, otherUser)
          They -> (otherUser, currentUser)
      (payerShare, budyShare) =
        case expensePayer expense of
          Me -> (myPart split, theirPart split)
          They -> (theirPart split, myPart split)
   in do _ <-
           execute
             conn
             "INSERT INTO expenses (payer, buddy, payer_share, buddy_share, amount) VALUES (?, ?, ?, ?, ?)"
             (show payer, show buddy, payerShare, budyShare, value $ expenseAmount expense)
         return ()
