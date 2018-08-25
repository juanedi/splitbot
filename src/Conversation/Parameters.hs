module Conversation.Parameters
  ( Amount(..)
  , Who(..)
  , Split(..)
  , readAmount
  , readWho
  , readSplit
  ) where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Text.Read (readMaybe)
import Text.Trifecta

newtype Amount = Amount
  { value :: Int
  }

data Who
  = Me
  | They

newtype Split = Split
  { myPart :: Integer
  } deriving (Show)

readAmount :: String -> Maybe Amount
readAmount str = fmap Amount $ (readMaybe str)

readWho :: String -> Maybe Who
readWho str =
  case (map toLower) str -- TODO: this should be defined in the same place as the keyboard options
        of
    "me" -> Just Me
    "they" -> Just They
    _ -> Nothing

readSplit :: String -> Maybe Split
readSplit str =
  case parseString splitParser mempty (map toLower str) of
    Success split -> Just split
    Failure _ -> Nothing

splitParser :: Parser Split
splitParser =
  (try (constParser "evenly" (Split 50)) <?> "tried 'evenly'") <|>
  (try (constParser "all on me" (Split 100)) <?> "tried 'all on me'") <|>
  (try (constParser "all on them" (Split 0)) <?> "tried 'all on them'") <|>
  myShareParser

myShareParser :: Parser Split
myShareParser = do
  who <- whoParser
  _ <- string " paid "
  share <- decimal
  _ <- string "%"
  if 0 <= share && share <= 100
    then case who of
           Me -> return (Split share)
           They -> return (Split (100 - share))
    else fail "Share must be between 0% and 100%"

whoParser :: Parser Who
whoParser =
  (try (constParser "i" Me) <?> "tried 'I'") <|> constParser "they" They

constParser :: String -> a -> Parser a
constParser accepts value = string accepts >> return value
