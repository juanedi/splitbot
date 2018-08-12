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

import Data.Char (toLower)
import Telegram (Reply(..), ReplyKeyboard(..))
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
  = Answer Reply
  | StoreAndReply Expense
                  Reply

data Expense = Expense
  { expensePayer :: Payer
  , expenseAmount :: Amount
  , expenseSplit :: Split
  }

start :: (Maybe Conversation, [Effect])
start = (Just AwaitingAmount, [Answer askAmount])

advance :: String -> Conversation -> (Maybe Conversation, [Effect])
advance userMessage conversation =
  case conversation of
    AwaitingAmount ->
      case readAmount userMessage of
        Just amount ->
          (Just $ AwaitingPayer {amount = amount}, [Answer askWhoPaid])
        Nothing -> (Just conversation, [Answer $ apologizing askAmount])
    AwaitingPayer amount ->
      case readPayer userMessage of
        Just payer ->
          ( Just $ AwaitingSplit {amount = amount, payer = payer}
          , [Answer askHowToSplit])
        Nothing -> (Just conversation, [Answer $ apologizing askWhoPaid])
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
        Nothing -> (Just conversation, [Answer $ apologizing askHowToSplit])

askAmount :: Reply
askAmount = Reply "How much?" Normal

readAmount :: String -> Maybe Amount
readAmount str = fmap Amount $ (readMaybe str)

askWhoPaid :: Reply
askWhoPaid = Reply "Who paid?" (Options ["Me", "Them"])

readPayer :: String -> Maybe Payer
readPayer str =
  case (map toLower) str -- TODO: this should be defined in the same place as the keyboard options
        of
    "me" -> Just Me
    "they" -> Just They
    _ -> Nothing

askHowToSplit :: Reply
askHowToSplit =
  Reply
    "How will you split it?"
    (Options ["Evenly", "All on me", "All on them"])

readSplit :: String -> Maybe Split
readSplit str =
  case (map toLower) str -- TODO: this should be defined in the same place as the keyboard options
        of
    "evenly" -> Just $ Split 50 50
    "all on me" -> Just $ Split 100 0
    _
      -- TODO: parse things such as "40% me / %30 them"
     -> Nothing

done :: Reply
done = Reply "Done!" Normal

apologizing :: Reply -> Reply
apologizing (Reply text options) =
  Reply ("Sorry, I couldn't understand that. " ++ text) options
