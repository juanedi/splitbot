module Main where

import Data.ByteString.Char8 (pack)
import Data.Traversable (sequence)
import Database.PostgreSQL.Simple
import System.Environment (getEnv)
import Migrations (migrateDB)
import Conversation
import qualified Telegram
import Control.Arrow ((>>>))

data State =
  State
    { currentConnection :: Connection
    , userA :: UserState
    , userB :: UserState
    , telegramState :: Telegram.State
    }

data Username = Username { toString :: String }

data UserId
  = UserA
  | UserB deriving Show

data UserState =
  UserState
    { username :: Username
    , buddy :: Username
    , conversation :: Maybe Conversation
    }

main :: IO ()
main = do
  userA <- getEnv "USER_A"
  userB <- getEnv "USER_B"
  url <- getEnv "DB_URL"
  telegramToken <- getEnv "TELEGRAM_TOKEN"
  conn <- connectPostgreSQL (pack url)
  migrateDB conn
  runServer $
    State
      { currentConnection = conn
      , userA = UserState (Username userA) (Username userB) Nothing
      , userB = UserState (Username userB) (Username userA) Nothing
      , telegramState = Telegram.init telegramToken
      }

runServer :: State -> IO ()
runServer state = do
  (message, newState) <- getMessage state
  let username = Telegram.username message
  let maybeUserId = matchUserId state username
  case maybeUserId of
    Nothing ->
      do
        putStrLn $ "Ignoring message from unknown user" ++ username
        runServer newState
    Just userId ->
        processMessage newState userId message >>= runServer

getMessage :: State -> IO (Telegram.Message, State)
getMessage state = do
  (message, updatedState) <- Telegram.getMessage (telegramState state)
  putStrLn $ "<< " ++ (show message)
  return $
    ( message
    , state { telegramState = updatedState }
    )

matchUserId :: State -> String -> Maybe UserId
matchUserId state uname
  | (userA >>> username >>> toString) state == uname = Just UserA
  | (userB >>> username >>> toString) state == uname = Just UserB
  | otherwise = Nothing

processMessage :: State -> UserId -> Telegram.Message -> IO (State)
processMessage state userId message =
  let
    currentUserState =
      getUserState userId state

    (updatedConversation, effects) =
      case conversation currentUserState of
        Nothing ->
          start

        Just conv ->
          advance conv (Telegram.text message)

    updatedUserState =
      currentUserState { conversation = updatedConversation }

    updatedState =
      setUserState
        userId
        updatedUserState
        state
  in
    do
      _ <- sequence (Prelude.map (runEffect state updatedUserState (Telegram.chatId message)) effects)
      return updatedState

getUserState :: UserId -> State -> UserState
getUserState userId =
  case userId of
    UserA -> userA
    UserB -> userB

setUserState :: UserId -> UserState -> State -> State
setUserState userId newUserState state=
  case userId of
    UserA -> state { userA = newUserState }
    UserB -> state { userB = newUserState }

runEffect :: State -> UserState -> Telegram.ChatId -> Effect -> IO ()
runEffect state userState chatId effect =
  case effect of
    Ask question ->
      sendMessage
          state
          chatId
          (questionText question)

    ApologizeAndAsk question ->
      sendMessage
        state
        chatId
        (apologizing $ questionText question)

    StoreAndConfirm expense ->
      do
        createExpense (currentConnection state) (username userState) (buddy userState) expense
        sendMessage
          state
          chatId
          "Done!"

sendMessage :: State -> Telegram.ChatId -> String -> IO ()
sendMessage state chatId text = do
  putStrLn $ ">> " ++ text
  Telegram.sendMessage
    (telegramState state)
    chatId
    text

apologizing :: String -> String
apologizing message =
  "Sorry, I couldn't understand that. " ++ message

questionText :: Question -> String
questionText question =
  case question of
    AskAmount ->
      "How much?"

    AskWhoPaid ->
      "Who paid?"

    AskHowToSplit ->
      "How will you split it?"

createExpense :: Connection -> Username -> Username -> Expense -> IO ()
createExpense conn currentUser otherUser expense =
  let
    split = expenseSplit expense

    (payer, buddy) =
      case expensePayer expense of
        Me ->
          (currentUser, otherUser)
        They ->
          (otherUser, currentUser)

    (payerShare, budyShare) =
      case expensePayer expense of
        Me ->
          ( myPart split, theirPart split)

        They ->
          ( theirPart split, myPart split)
  in
    do
      _ <- execute conn
            "INSERT INTO expenses (payer, buddy, payer_share, buddy_share, amount) VALUES (?, ?, ?, ?, ?)"
            ( toString $ payer
            , toString $ buddy
            , payerShare
            , budyShare
            , value $ expenseAmount expense
            )
      return ()
