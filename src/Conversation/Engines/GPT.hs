{-# LANGUAGE TemplateHaskell #-}

module Conversation.Engines.GPT (Conversation.Engines.GPT.init, PromptParams (..)) where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Conversation.Expense (Expense)
import qualified Conversation.Expense as Expense
import Conversation.Outcome (Outcome)
import qualified Conversation.Outcome as Outcome
import Data.Aeson (FromJSON, parseJSON, withText)
import qualified Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text
import Data.Text.Encoding (encodeUtf8)
import Dhall (ToDhall)
import qualified Dhall
import GHC.Generics (Generic)
import OpenAI (ChatMessage, ChatModel (..), ChatParams)
import qualified OpenAI
import System.FilePath.Posix (FilePath)
import qualified Telegram.Reply as Reply


newtype State = State
  { messages :: [ChatMessage]
  }


data PromptParams = PromptParams
  { userName :: String
  , partnerName :: String
  , botName :: String
  }
  deriving (Eq, Generic, Show)


instance ToDhall PromptParams


data SessionState = InProgress | Done


instance FromJSON SessionState where
  parseJSON =
    withText
      "SessionState"
      ( \text ->
          case text of
            "in_progress" -> return InProgress
            "done" -> return Done
            _ -> fail ("Unrecognized state: " ++ Data.Text.unpack text)
      )


data Intent = AskTitle | AskWhoPaid | AskCost | AskSplit | Other


instance FromJSON Intent where
  parseJSON =
    withText
      "Intent"
      ( \text ->
          case text of
            "ask_title" -> return AskTitle
            "ask_who_paid" -> return AskWhoPaid
            "ask_cost" -> return AskCost
            "ask_split" -> return AskSplit
            "other" -> return Other
            _ -> fail ("Unrecognized intent: " ++ Data.Text.unpack text)
      )


data WhoPaid = User | Partner


$( deriveJSON
    (defaultOptions {constructorTagModifier = map toLower})
    ''WhoPaid
 )


data ExpenseDraft = ExpenseDraft
  { title :: Maybe Text
  , whoPaid :: Maybe WhoPaid
  , cost :: Maybe Float
  , -- TODO: account for 'proportionally'
    split :: Maybe Float
  }
  deriving (Generic)


instance FromJSON ExpenseDraft


data GPTReply = GPTReply
  { state :: SessionState
  , response :: Text
  , intent :: Intent
  , expense :: ExpenseDraft
  }
  deriving (Generic)


instance FromJSON GPTReply


init :: OpenAI.Handler -> FilePath -> PromptParams -> IO (Text -> IO Outcome)
init openAI promptTemplatePath promptParams = do
  promptTemplate <- Dhall.input Dhall.auto (Data.Text.pack promptTemplatePath)

  let prompt =
        OpenAI.ChatMessage
          { OpenAI.content = promptTemplate promptParams
          , OpenAI.role = OpenAI.User
          }

  stateVar <- newMVar (State [prompt])

  pure $ \message -> do
    modifyMVar
      stateVar
      (onMessage openAI message)


onMessage :: OpenAI.Handler -> Text -> State -> IO (State, Outcome)
onMessage openAI message s = do
  let withUserMessage =
        addMessage
          (OpenAI.ChatMessage {OpenAI.content = message, OpenAI.role = OpenAI.User})
          (messages s)
  result <-
    OpenAI.chat
      openAI
      ( OpenAI.ChatParams
          { OpenAI.model = GPT_3_5_Turbo
          , OpenAI.temperature = 0.7
          , OpenAI.messages = withUserMessage
          }
      )
  case result of
    Left err ->
      pure
        (s, Outcome.Continue (Reply.plain ("Ooops something went wrong 😅" ++ show err)))
    Right botMessage ->
      case Data.Aeson.eitherDecode (textToByteString (OpenAI.content botMessage)) of
        Left err ->
          -- TODO: nudge the bot and try again?
          pure (s, Outcome.Continue (Reply.plain ("Ooops something went wrong 😅" ++ show err)))
        Right reply ->
          pure
            ( State (addMessage botMessage (messages s))
            , case state reply of
                InProgress ->
                  Outcome.Continue (Reply.plain (Data.Text.unpack (response reply)))
                Done ->
                  case checkExpense (expense reply) of
                    Nothing ->
                      Outcome.Continue (Reply.plain (Data.Text.unpack (response reply)))
                    Just e ->
                      Outcome.Done e
            )


checkExpense :: ExpenseDraft -> Maybe Expense
checkExpense draft =
  initExpense
    <$> title draft
    <*> whoPaid draft
    <*> cost draft
    <*> split draft
  where
    initExpense title_ whoPaid_ cost_ split_ =
      Expense.Expense
        { Expense.description =
            Expense.Description (Data.Text.unpack title_)
        , Expense.payer =
            case whoPaid_ of
              User -> Expense.Me
              Partner -> Expense.They
        , Expense.amount =
            Expense.Amount (floor cost_)
        , Expense.split =
            Expense.Split (floor split_)
        }


addMessage :: ChatMessage -> [ChatMessage] -> [ChatMessage]
addMessage message messages =
  messages ++ [message]


textToByteString :: Text -> LBS.ByteString
textToByteString =
  -- TODO: maybe we should be using bytestring instead of text?
  LBS.fromChunks . (: []) . encodeUtf8
