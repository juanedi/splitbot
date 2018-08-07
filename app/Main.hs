module Main where

import Data.ByteString.Char8 (pack)
import Data.Traversable (sequence)
import qualified Data.Map.Strict as Map
import Database.PostgreSQL.Simple
import System.Environment (getEnv)
import Migrations (migrateDB)
import Conversation
import qualified Telegram

data UserId = UserA | UserB deriving (Show, Eq, Ord)

data State =
  State
    { currentConnection :: Connection
    , conversations :: Map.Map UserId Conversation
    , telegramState :: Telegram.State
    }

data Message = Message UserId String

main :: IO ()
main = do
  url <- getEnv "DB_URL"
  telegramToken <- getEnv "TELEGRAM_TOKEN"
  conn <- connectPostgreSQL (pack url)
  migrateDB conn
  runServer $
    State
      { currentConnection = conn
      , conversations = Map.empty
      , telegramState = Telegram.init telegramToken
      }

runServer :: State -> IO ()
runServer state = do
  (message, newState) <- readMessage state
  newState <- processMessage newState message
  runServer newState

readMessage :: State -> IO (Message, State)
readMessage state = do
  (update, updatedState) <- Telegram.getUpdate (telegramState state)
  putStrLn $ "<< " ++ (Telegram.text update)
  return $
    (Message
        (userId (Telegram.username update))
        (Telegram.text update)
    , state { telegramState = updatedState }
    )

userId :: String -> UserId
userId =
  -- TODO: read from config
  const UserA

sendMessage :: String -> IO ()
sendMessage message =
  putStrLn $ ">> " ++ message

processMessage :: State -> Message -> IO (State)
processMessage state (Message userId text) =
  let
    currentConversation =
      Map.lookup userId (conversations state)

    (updatedConversation, effects) =
      case currentConversation of
        Nothing ->
          start

        Just conversation ->
          advance conversation text

    updatedConversations =
      Map.alter (const updatedConversation) userId (conversations state)
  in
    do
      _ <- sequence (Prelude.map (runEffect state userId) effects)
      return $ state { conversations = updatedConversations }

runEffect :: State -> UserId -> Effect -> IO ()
runEffect state currentUser effect =
  case effect of
    Ask question ->
      sendMessage (questionText question)

    ApologizeAndAsk question ->
      sendMessage (apologizing $ questionText question)

    StoreAndConfirm expense ->
      do
        createExpense (currentConnection state) currentUser expense
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

createExpense :: Connection -> UserId -> Expense -> IO ()
createExpense conn currentUser expense =
  let
    otherUser userId =
      case userId of
        UserA -> UserB
        UserB -> UserA

    split = expenseSplit expense

    (payer, buddy) =
      case expensePayer expense of
        Me ->
          (currentUser, otherUser currentUser)
        They ->
          (otherUser currentUser, currentUser)

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
            ( show $ payer
            , show $ buddy
            , payerShare
            , budyShare
            , value $ expenseAmount expense
            )
      return ()
