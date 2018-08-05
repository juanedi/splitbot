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
import Conversation (Conversation, Effect(..), Question(..), start, advance)

data Config =
  Config
    { dbUrl :: String
    }

data CreateCommand =
  CreateCommand
    { payer       :: String
    , buddy       :: String
    , payerShare  :: Int
    , buddyShare  :: Int
    , amount      :: Int
    }

data UserId = UserA | UserB

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
  let state = State conn UserA Nothing
  runServer state

runServer :: State -> IO ()
runServer state = do
  putStr "> "
  hFlush stdout
  command <- getLine
  newState <- processCommand state command
  runServer newState

processCommand :: State -> String -> IO (State)
processCommand state message =
  let
    (conversationState, effects) =
      case conversation state of
        Nothing ->
          start

        Just conversation ->
          advance conversation message
  in
    do
      sequence (map runEffect effects)
      return $ state { conversation = conversationState }

runEffect :: Effect -> IO ()
runEffect effect =
  case effect of
    Ask question ->
      sendMessage $
        case question of
          AskAmount ->
            "How much?"

          AskWhoPaid ->
            "Who paid?"

          AskHowToSplit ->
            "How will you split it?"

    StoreAndConfirm _ _ _ ->
      do
        -- TODO: store in DB
        sendMessage "Done!"

sendMessage :: String -> IO ()
sendMessage =
  putStrLn

createExpense :: Connection -> CreateCommand -> IO ()
createExpense conn command = do
  execute conn
    "INSERT INTO expenses (payer, buddy, payer_share, buddy_share, amount) VALUES (?, ?, ?, ?, ?)"
    ( payer command
    , buddy command
    , payerShare command
    , buddyShare command
    , amount command
    )
  return ()
