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
    { payer      :: String
    , budy       :: String
    , payerShare :: Int
    , amount     :: Int
    }

main :: IO ()
main = do
  migrateDB

migrateDB :: IO ()
migrateDB = do
  putStrLn "--- Migrating database"
  url <- getEnv "DB_URL"
  conn <- connectPostgreSQL (pack url)
  withTransaction conn $
    runMigrations True conn $
      [ MigrationInitialization
      , (MigrationDirectory "./db/migrate/")
      ]
  return ()

createExpense :: CreateCommand -> IO ()
createExpense command =
  return ()
