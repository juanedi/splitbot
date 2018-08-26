module Storage
  ( migrateDB
  , createExpense
  ) where

import Conversation (Expense(..))
import Conversation.Parameters (Amount(..), Split(..), Who(..))
import Database.PostgreSQL.Simple (Connection, execute, withTransaction)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand(..)
  , runMigrations
  )
import Telegram (Username)

migrateDB :: Connection -> IO ()
migrateDB conn = do
  _ <-
    withTransaction conn $
    runMigrations True conn $
    [MigrationInitialization, (MigrationDirectory "./db/migrate/")]
  return ()

createExpense ::
     Connection -> Telegram.Username -> Telegram.Username -> Expense -> IO ()
createExpense conn currentUser otherUser expense =
  let split = expenseSplit expense
      (payerShare, payer, buddy) =
        case expensePayer expense of
          Me -> (myPart split, currentUser, otherUser)
          They -> (100 - myPart split, otherUser, currentUser)
   in do _ <-
           execute
             conn
             "INSERT INTO expenses (amount, payer_share, payer, peer) VALUES (?, ?, ?, ?)"
             (value $ expenseAmount expense, payerShare, show payer, show buddy)
         return ()
