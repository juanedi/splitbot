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
      , userA = UserState (Username userA) Nothing
      , userB = UserState (Username userB) Nothing
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
  return $
    ( message
    , state { telegramState = updatedState }
    )

matchUserId :: State -> String -> Maybe UserId
matchUserId state uname
  | (userA >>> username >>> toString) state == uname = Just UserA
  | (userB >>> username >>> toString) state == uname = Just UserB
  | otherwise = Nothing

sendMessage :: String -> IO ()
sendMessage message =
  putStrLn $ ">> " ++ message

processMessage :: State -> UserId -> Telegram.Message -> IO (State)
processMessage state userId message =
  let
    currentConversation =
      case userId of
        UserA -> (userA >>> conversation) state
        UserB -> (userB >>> conversation) state

    (updatedConversation, effects) =
      case currentConversation of
        Nothing ->
          start

        Just conversation ->
          advance conversation (Telegram.text message)

    updatedState =
      case userId of
        UserA -> state { userA = (userA state) { conversation = updatedConversation }}
        UserB -> state { userB = (userB state) { conversation = updatedConversation }}
  in
    do
      _ <- sequence (Prelude.map (runEffect state userId) effects)
      return updatedState

runEffect :: State -> UserId -> Effect -> IO ()
runEffect state currentUserId effect =
  case effect of
    Ask question ->
      sendMessage (questionText question)

    ApologizeAndAsk question ->
      sendMessage (apologizing $ questionText question)

    StoreAndConfirm expense ->
      let
        (currentUsername, otherUsername) =
          case currentUserId of
            UserA -> ((userA >>> username) state, (userB >>> username) state)
            UserB -> ((userB >>> username) state, (userA >>> username) state)
      in do
        createExpense (currentConnection state) currentUsername otherUsername expense
        sendMessage "Done!"

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
