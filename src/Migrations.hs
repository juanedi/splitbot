module Migrations where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration

migrateDB :: Connection -> IO ()
migrateDB conn = do
  _ <-
    withTransaction conn $
    runMigrations True conn $
    [MigrationInitialization, (MigrationDirectory "./db/migrate/")]
  return ()
