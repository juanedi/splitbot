{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import System.Environment (getEnv)
import System.Exit (exitSuccess)

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

main :: IO ()
main = do
  url <- getEnv "DB_URL"
  conn <- connectPostgreSQL (pack url)
  migrateDB conn
  createExpense conn $
    CreateCommand "foo" "bar" 40 60 1000

migrateDB :: Connection -> IO ()
migrateDB conn = do
  withTransaction conn $
    runMigrations True conn $
      [ MigrationInitialization
      , (MigrationDirectory "./db/migrate/")
      ]
  return ()

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
