module Conversation.Parameters.Split
  ( Split(..)
  , ask
  , parse
  ) where

import Control.Applicative ((<|>))
import Conversation.Parameters.Definitions
import Data.Char (toLower)
import Telegram (Reply(..), ReplyKeyboard(..))
import Text.Trifecta

newtype Split = Split
  { myPart :: Integer
  } deriving (Show)

ask :: Split -> Reply
ask preset = Reply
  "How will you split it?"
  (Options
    [ "Evenly"
    , "All on me"
    , "All on them"
    , "I paid " ++ show (myPart preset) ++ "%"
    ]
  )

parse :: String -> Maybe Split
parse str = case parseReply str of
  Success split -> Just split
  Failure _     -> Nothing

parseReply :: String -> Result Split
parseReply str = parseString (parser <* eof) mempty (map toLower str)

parser :: Parser Split
parser =
  (try (constParser "evenly" (Split 50)) <?> "tried 'evenly'")
    <|> (try (constParser "all on me" (Split 100)) <?> "tried 'all on me'")
    <|> (try (constParser "all on them" (Split 0)) <?> "tried 'all on them'")
    <|> myShareParser

myShareParser :: Parser Split
myShareParser = do
  who   <- whoParser
  _     <- string " paid "
  share <- decimal
  _     <- string "%"
  if 0 <= share && share <= 100
    then case who of
      Me   -> return (Split share)
      They -> return (Split (100 - share))
    else fail "Share must be between 0% and 100%"

whoParser :: Parser Who
whoParser =
  (try (constParser "i" Me) <?> "tried 'I'") <|> constParser "they" They

constParser :: String -> a -> Parser a
constParser accepts value = string accepts >> return value
