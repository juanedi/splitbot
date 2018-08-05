{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Char8 (pack)
import Data.Traversable (sequence)
import Database.PostgreSQL.Simple
import Control.Concurrent (threadDelay)
import System.Environment (getEnv)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Migrations (migrateDB)
import Conversation

data UserId = UserA | UserB deriving Show

data State =
  State
    { conn :: Connection
    , userId :: UserId
    , conversation :: Maybe Conversation
    }

main :: IO ()
main = do
  url <- getEnv "DB_URL"
  conn <- connectPostgreSQL (pack url)
  migrateDB conn
  runServer $
    State
      { conn = conn
      , userId = UserA
      , conversation = Nothing
      }

runServer :: State -> IO ()
runServer state = do
  message <- readMessage
  newState <- processMessage state message
  runServer newState

readMessage :: IO (String)
readMessage = do
  putStr "> "
  hFlush stdout
  getLine

processMessage :: State -> String -> IO (State)
processMessage state message =
  let
    (conversationState, effects) =
      case conversation state of
        Nothing ->
          start

        Just conversation ->
          advance conversation message
  in
    do
      sequence (map (runEffect state) effects)
      return $ state { conversation = conversationState }

runEffect :: State -> Effect -> IO ()
runEffect state effect =
  case effect of
    Ask question ->
      sendMessage (questionText question)

    ApologizeAndAsk question ->
      sendMessage (apologizing $ questionText question)

    StoreAndConfirm expense ->
      do
        createExpense (conn state) (userId state) expense
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

sendMessage :: String -> IO ()
sendMessage =
  putStrLn

createExpense :: Connection -> UserId -> Expense -> IO ()
createExpense conn userId expense =
  let
    otherUser userId =
      case userId of
        UserA -> UserB
        UserB -> UserA

    split = expenseSplit expense

    (payer, buddy) =
      case expensePayer expense of
        Me ->
          (userId, otherUser userId)
        They ->
          (otherUser userId, userId)

    (payerShare, budyShare) =
      case expensePayer expense of
        Me ->
          ( myPart split, theirPart split)

        They ->
          ( theirPart split, myPart split)
  in
    do
      execute conn
        "INSERT INTO expenses (payer, buddy, payer_share, buddy_share, amount) VALUES (?, ?, ?, ?, ?)"
        ( show $ payer
        , show $ buddy
        , payerShare
        , budyShare
        , value $ expenseAmount expense
        )
      return ()
