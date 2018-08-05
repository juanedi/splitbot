{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple
import Control.Concurrent (threadDelay)
import System.Environment (getEnv)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Migrations (migrateDB)
import Conversation (Conversation, new, User(..))

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

data State =
  State
    { conn :: Connection
    , user :: Conversation.User
    , conversation :: Conversation
    }

main :: IO ()
main = do
  url <- getEnv "DB_URL"
  conn <- connectPostgreSQL (pack url)
  migrateDB conn
  let state = State conn (User "juanedi") Conversation.new
  runServer state
  -- createExpense conn $
  --   CreateCommand "foo" "bar" 40 60 1000

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

runServer :: State -> IO ()
runServer state = do
  putStr "> "
  hFlush stdout
  command <- getLine
  newState <- processCommand state command
  runServer newState

processCommand :: State -> String -> IO (State)
processCommand state command = do
  putStrLn ("Ok " ++ (show $ user state) ++ "!")
  return state
