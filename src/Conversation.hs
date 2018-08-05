module Conversation (User(..), Conversation, new) where

newtype User = User String deriving Show

newtype Amount = Amount Int

data Payer = Me | They

data Split
  = Equally
  | Unequally (Int, Int)

data Conversation
  = New
  | AwaitingBuddy
  | AwaitingAmount { buddy :: User }
  | AwaitingPayer  { buddy :: User, amount :: Amount }
  | AwaitingSplit  { payer :: Payer, buddy :: User, amount :: Amount }

data Message
  = Init User Int
  | SetPayer Payer
  | SetSplit Split

data Reply
  = Answer Conversation String
  | AnswerAndTerminate String

data Effect
  = StoreExpense
    { expensePayer  :: (User, Int)
    , expenseBuddy  :: (User, Int)
    , expenseAmount :: Amount
    }
  | NoEffect

new :: Conversation
new = New

advance :: Conversation -> String -> (Reply, Effect)
advance conversation userMessage =
  ( AnswerAndTerminate "Alright!"
  , NoEffect
  )
