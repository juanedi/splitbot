{- | Engine used to gather information from the user.

 Just an intermediate layer for now, but meant to serve as a boundary to
 abstract over legacy vs AI-based querying.
-}
module Conversation.Engine (State, Outcome (..), start, update) where

import Control.Applicative ((<|>))
import Conversation.Expense (Amount (..), Description (..), Expense)
import qualified Conversation.Expense as Expense
import Conversation.Parameters.Split (Split)
import qualified Conversation.Parameters.Split as Split
import Conversation.Parameters.Who (Who)
import qualified Conversation.Parameters.Who as Who
import Data.Char (toLower)
import Telegram.Reply (Reply)
import qualified Telegram.Reply
import Text.Read (readMaybe)
import Text.Trifecta (
  Parser,
  decimal,
  eof,
  parseString,
  string,
  try,
  (<?>),
 )
import qualified Text.Trifecta


data State
  = -- Initial state when user first contacts the bot via the '/start' command
    AwaitingDescription
      { preset :: Split
      }
  | -- Initial state when the user contacts the bot sending a description
    AwaitingInitialConfirmation
      { preset :: Split
      , description :: Description
      }
  | AwaitingAmount
      { preset :: Split
      , description :: Description
      }
  | AwaitingPayer
      { preset :: Split
      , description :: Description
      , amount :: Amount
      }
  | AwaitingSplit
      { preset :: Split
      , description :: Description
      , payer :: Who
      , amount :: Amount
      }


data Outcome
  = Continue State Reply
  | Terminate Reply
  | Confirm Expense


start :: String -> Split -> (State, Reply)
start message preset =
  case message of
    "/start" ->
      ( AwaitingDescription preset
      , Telegram.Reply.plain "👋 Hey! Please enter a description for the expense report."
      )
    _ ->
      let description = Description message
       in ( AwaitingInitialConfirmation preset description
          , confirmDescription description
          )


update :: String -> State -> Outcome
update userMessage state =
  case state of
    AwaitingDescription preset ->
      continue
        ( AwaitingAmount
            { preset = preset
            , description = Description userMessage
            }
        )
        (Telegram.Reply.plain "How much?")
    AwaitingInitialConfirmation preset description ->
      if userMessage == "Yes"
        then
          continue
            (AwaitingAmount {preset = preset, description = description})
            (Telegram.Reply.plain "How much?")
        else terminate
    AwaitingAmount preset description ->
      case parseAmount userMessage of
        Just amount ->
          continue
            ( AwaitingPayer
                { preset = preset
                , description = description
                , amount = amount
                }
            )
            askWho
        Nothing ->
          continue
            state
            (Telegram.Reply.apologizing askAmount)
    AwaitingPayer preset description amount ->
      case parseWho userMessage of
        Just payer ->
          continue
            ( AwaitingSplit
                { preset = preset
                , description = description
                , amount = amount
                , payer = payer
                }
            )
            ( Telegram.Reply.withOptions
                "How will you split it?"
                ["Evenly", "All on me", "All on them", show (Split.myPart preset) ++ "% on me"]
            )
        Nothing ->
          continue
            state
            askWho
    AwaitingSplit preset description payer amount ->
      case parseSplit userMessage of
        Just split ->
          let expense =
                ( Expense.Expense
                    { Expense.description = description
                    , Expense.payer = payer
                    , Expense.amount = amount
                    , Expense.split = split
                    }
                )
           in confirm expense
        Nothing ->
          continue
            state
            (Telegram.Reply.apologizing (askSplit preset))


continue :: State -> Reply -> Outcome
continue = Continue


confirm :: Expense -> Outcome
confirm = Confirm


terminate :: Outcome
terminate = Terminate (Telegram.Reply.plain "Alright, the expense was discarded 👍")


confirmDescription :: Description -> Reply
confirmDescription (Description description) =
  Telegram.Reply.withOptions
    ( concat
        [ "👋 Hey! We'll create an expense report for \""
        , description
        , "\". Is that correct?"
        ]
    )
    ["Yes", "No"]


askAmount :: Reply
askAmount =
  Telegram.Reply.plain "How much?"


parseAmount :: String -> Maybe Amount
parseAmount str = Amount <$> readMaybe str


askWho :: Reply
askWho =
  Telegram.Reply.withOptions "Who paid?" ["Me", "They"]


parseWho :: String -> Maybe Who
parseWho str =
  case map toLower str of
    "me" ->
      Just Who.Me
    "they" ->
      Just Who.They
    _ ->
      Nothing


askSplit :: Split -> Reply
askSplit preset =
  Telegram.Reply.withOptions
    "How will you split it?"
    ["Evenly", "All on me", "All on them", show (Split.myPart preset) ++ "% on me"]


parseSplit :: String -> Maybe Split
parseSplit str =
  case parseSplit' str of
    Text.Trifecta.Success split -> Just split
    Text.Trifecta.Failure _ -> Nothing


parseSplit' :: String -> Text.Trifecta.Result Split
parseSplit' str = parseString (splitParser <* eof) mempty (map toLower str)


splitParser :: Parser Split
splitParser =
  (try (constParser "evenly" (Split.Split 50)) <?> "tried 'evenly'")
    <|> (try (constParser "all on me" (Split.Split 100)) <?> "tried 'all on me'")
    <|> (try (constParser "all on them" (Split.Split 0)) <?> "tried 'all on them'")
    <|> myShareParser


myShareParser :: Parser Split
myShareParser = do
  share <- decimal
  _ <- string "% on "
  who <- whoParser
  if 0 <= share && share <= 100
    then case who of
      Who.Me -> return (Split.Split share)
      Who.They -> return (Split.Split (100 - share))
    else fail "Share must be between 0% and 100%"


whoParser :: Parser Who
whoParser =
  (try (constParser "me" Who.Me) <?> "tried 'I'") <|> constParser "them" Who.They


constParser :: String -> a -> Parser a
constParser accepts value = string accepts >> return value


askConfirmation :: Expense -> Reply
askConfirmation expense =
  let descriptionLine =
        concat ["*", (Expense.descriptionText . Expense.description) expense, "*\n"]

      payerLine =
        concat
          [ "Payed by "
          , case Expense.payer expense of
              Who.Me -> "me"
              Who.They -> "them"
          , "\n"
          ]

      amountLine =
        concat ["Total: $", show $ (Expense.amountValue . Expense.amount) expense, "\n"]

      splitLine =
        concat ["I owe ", show $ Split.myPart (Expense.split expense), "%", "\n"]
   in Telegram.Reply.withOptions
        ( concat
            [ "Is this correct❓\n\n"
            , descriptionLine
            , amountLine
            , payerLine
            , splitLine
            ]
        )
        ["Yes", "No"]
