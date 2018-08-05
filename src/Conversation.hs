module Conversation
  ( Amount(..),
    Conversation,
    Effect(..),
    Expense(..),
    Question(..),
    Payer(..),
    Split(..),
    advance,
    start
  ) where

data Conversation
  = AwaitingAmount
  | AwaitingPayer  { amount :: Amount }
  | AwaitingSplit  { payer :: Payer, amount :: Amount }

data Payer = Me | They

newtype Amount = Amount { value :: Int }

data Split
  = Split
    { myPart    :: Int
    , theirPart :: Int
    }

data Question
  = AskAmount
  | AskWhoPaid
  | AskHowToSplit

data Effect
  = Ask Question
  | StoreAndConfirm Expense

data Expense =
  Expense
    { expensePayer  :: Payer
    , expenseAmount :: Amount
    , expenseSplit  :: Split
    }


start :: (Maybe Conversation, [Effect])
start =
  ( Just AwaitingAmount , [ Ask AskAmount ])

advance :: Conversation -> String -> (Maybe Conversation, [Effect])
advance conversation userMessage =
  case conversation of
    AwaitingAmount ->
      ( Just $ AwaitingPayer { amount = Amount 100 }
      , [ Ask AskWhoPaid ]
      )

    AwaitingPayer amount ->
      ( Just $ AwaitingSplit { amount = amount, payer = Me }
      , [ Ask AskHowToSplit ]
      )

    AwaitingSplit payer amount ->
      ( Nothing
      , [ StoreAndConfirm $
            Expense
              { expensePayer = payer
              , expenseAmount = amount
              , expenseSplit = (Split 40 60)
              }
        ]
      )
