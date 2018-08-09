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

import Conversation.Replies
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
  = Reply String
  | StoreAndReply Expense
                  String

data Expense = Expense
  { expensePayer :: Payer
  , expenseAmount :: Amount
  , expenseSplit :: Split
  }

start :: (Maybe Conversation, [Effect])
start = (Just AwaitingAmount, [Reply askAmount])

advance :: String -> Conversation -> (Maybe Conversation, [Effect])
advance userMessage conversation =
  case conversation of
    AwaitingAmount ->
      case readAmount userMessage of
        Just amount ->
          (Just $ AwaitingPayer {amount = amount}, [Reply askWhoPaid])
        Nothing -> (Just conversation, [Reply $ apologizing askAmount])
    AwaitingPayer amount ->
      case readPayer userMessage of
        Just payer ->
          ( Just $ AwaitingSplit {amount = amount, payer = payer}
          , [Reply askHowToSplit])
        Nothing -> (Just conversation, [Reply $ apologizing askWhoPaid])
    AwaitingSplit payer amount ->
      case readSplit userMessage of
        Just split ->
          ( Nothing
          , [ StoreAndReply
                (Expense
                   { expensePayer = payer
                   , expenseAmount = amount
                   , expenseSplit = split
                   })
                done
            ])
        Nothing -> (Just conversation, [Reply $ apologizing askHowToSplit])

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
