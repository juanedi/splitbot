module Storage where

import Conversation (Amount(..), Expense(..), Payer(..), Split(..))
import Database.PostgreSQL.Simple (Connection, execute)
import Telegram (Username)

createExpense ::
     Connection -> Telegram.Username -> Telegram.Username -> Expense -> IO ()
createExpense conn currentUser otherUser expense =
  let split = expenseSplit expense
      (payer, buddy) =
        case expensePayer expense of
          Me -> (currentUser, otherUser)
          They -> (otherUser, currentUser)
      (payerShare, budyShare) =
        case expensePayer expense of
          Me -> (myPart split, theirPart split)
          They -> (theirPart split, myPart split)
   in do _ <-
           execute
             conn
             "INSERT INTO expenses (payer, buddy, payer_share, buddy_share, amount) VALUES (?, ?, ?, ?, ?)"
             ( show payer
             , show buddy
             , payerShare
             , budyShare
             , value $ expenseAmount expense)
         return ()