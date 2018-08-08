module Conversation
  ( Amount(..)
  , Conversation
  , Effect(..)
  , Expense(..)
  , Question(..)
  , Payer(..)
  , Split(..)
  , advance
  , start
  ) where

import Text.Read (readMaybe)

data Conversation
  = AwaitingAmount
  | AwaitingPayer { amount :: Amount }
  | AwaitingSplit { payer :: Payer
                  , amount :: Amount }

data Payer
  = Me
  | They

newtype Amount = Amount
  { value :: Int
  }

data Split = Split
  { myPart :: Int
  , theirPart :: Int
  }

data Question
  = AskAmount
  | AskWhoPaid
  | AskHowToSplit

data Effect
  = Ask Question
  | ApologizeAndAsk Question
  | StoreAndConfirm Expense

data Expense = Expense
  { expensePayer :: Payer
  , expenseAmount :: Amount
  , expenseSplit :: Split
  }

start :: (Maybe Conversation, [Effect])
start = (Just AwaitingAmount, [Ask AskAmount])

advance :: String -> Conversation -> (Maybe Conversation, [Effect])
advance userMessage conversation =
  case conversation of
    AwaitingAmount ->
      case readAmount userMessage of
        Just amount ->
          (Just $ AwaitingPayer {amount = amount}, [Ask AskWhoPaid])
        Nothing -> (Just conversation, [ApologizeAndAsk AskAmount])
    AwaitingPayer amount ->
      case readPayer userMessage of
        Just payer ->
          ( Just $ AwaitingSplit {amount = amount, payer = payer}
          , [Ask AskHowToSplit])
        Nothing -> (Just conversation, [ApologizeAndAsk AskWhoPaid])
    AwaitingSplit payer amount ->
      case readSplit userMessage of
        Just split ->
          ( Nothing
          , [ StoreAndConfirm $
              Expense
                { expensePayer = payer
                , expenseAmount = amount
                , expenseSplit = split
                }
            ])
        Nothing -> (Just conversation, [ApologizeAndAsk AskHowToSplit])

readAmount :: String -> Maybe Amount
readAmount str = fmap Amount $ (readMaybe str)

readPayer :: String -> Maybe Payer
readPayer str =
  case str of
    "me" -> Just Me
    "they" -> Just They
    _ -> Nothing

readSplit :: String -> Maybe Split
readSplit str =
  case str of
    "evenly" -> Just $ Split 50 50
    "all on me" -> Just $ Split 100 0
    "all on them" -> Just $ Split 0 100
    _
      -- TODO: parse things such as "40% me / %30 them"
     -> Nothing
