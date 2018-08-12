module Telegram where

import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import Data.Aeson (eitherDecode, encode)
import Network.HTTP.Client
  ( Manager
  , Request
  , RequestBody(..)
  , httpLbs
  , method
  , parseRequest_
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  )
import Network.HTTP.Types.Status (statusCode)
import qualified Telegram.Api.GetUpdates as GetUpdates
import qualified Telegram.Api.SendMessage as SendMessage

data State = State
  { token :: Token
  , manager :: Manager
  , fetchState :: FetchState
  }

type Token = String

data Username =
  Username String

instance Eq Username where
  (==) (Username a) (Username b) = a == b

instance Show Username where
  show (Username a) = a

data FetchState
  = Buffered GetUpdates.Update
             [GetUpdates.Update]
  | NeedMore (Maybe Int)

data Message = Message
  { chatId :: ChatId
  , username :: Username
  , text :: String
  }

data Reply =
  Reply String
        ReplyKeyboard

data ReplyKeyboard
  = Normal
  | Options [String]

data ChatId =
  ChatId Integer
  deriving (Show)

init :: Token -> Manager -> State
init token manager =
  State {token = token, manager = manager, fetchState = NeedMore Nothing}

getMessage :: State -> IO (Message, State)
getMessage state =
  case fetchState state of
    Buffered nextUpdate rest ->
      return
        ( toMessage nextUpdate
        , case rest of
            u:us -> state {fetchState = Buffered u us}
            [] ->
              state
                {fetchState = NeedMore $ Just $ GetUpdates.updateId nextUpdate})
    NeedMore lastUpdateId -> do
      response <- requestUpdates (token state) (manager state) lastUpdateId
      case GetUpdates.result response of
        u:us -> getMessage $ state {fetchState = Buffered u us}
        [] -> do
          putStrLn "No updates found! Will retry in a bit"
          threadDelay (1 * 1000 * 1000)
          getMessage state

toMessage :: GetUpdates.Update -> Message
toMessage update =
  let message = GetUpdates.message update
      user = GetUpdates.from message
   in Message
        { chatId = ChatId (GetUpdates.id user)
        , username = Username (GetUpdates.username user)
        , text = GetUpdates.text message
        }

requestUpdates ::
     Token -> Manager -> Maybe Int -> IO (GetUpdates.UpdateResponse)
requestUpdates token manager lastUpdateId = do
  request <- newUpdateRequest token lastUpdateId
  response <- httpLbs request manager
  case (responseStatus >>> statusCode) response of
    200 ->
      case eitherDecode (responseBody response) of
        Left err -> do
          putStrLn "Decoding error! Skipping message"
          putStrLn err
          requestUpdates token manager ((+ 1) <$> lastUpdateId)
        Right update -> return update
    _ -> do
      putStrLn "Error contacting telegram for updates. Will retry soon."
      requestUpdates token manager lastUpdateId

newUpdateRequest :: Token -> Maybe Int -> IO Request
newUpdateRequest token lastUpdateId =
  return $
  (parseRequest_ (apiUrl token "getUpdates"))
    { method = "GET"
    , requestHeaders = [("Content-Type", "application/json")]
    , requestBody = RequestBodyLBS (encode updateRequest)
    }
  where
    updateRequest =
      GetUpdates.Request
        { GetUpdates.timeout = 10
        , GetUpdates.limit = 30
        , GetUpdates.allowedUpdates = ["message"]
        , GetUpdates.offset = (+ 1) <$> lastUpdateId
        }

apiUrl :: Token -> String -> String
apiUrl token apiMethod =
  concat ["https://api.telegram.org/bot", token, "/", apiMethod]

sendMessage :: State -> ChatId -> Reply -> IO ()
sendMessage state chatId reply = do
  request <- newSendRequest (token state) chatId reply
  response <- httpLbs request (manager state)
  case (responseStatus >>> statusCode) response of
    200 -> return ()
    _ -> do
      putStrLn "Error sending message"
      return ()

newSendRequest :: Token -> ChatId -> Reply -> IO Request
newSendRequest token (ChatId chatId) (Reply text keyboard) =
  return $
  (parseRequest_ (apiUrl token "sendMessage"))
    { method = "POST"
    , requestHeaders = [("Content-Type", "application/json")]
    , requestBody = (RequestBodyLBS . encode) message
    }
  where
    message = SendMessage.Message chatId text (replyMarkup keyboard)

replyMarkup :: ReplyKeyboard -> SendMessage.ReplyMarkup
replyMarkup keyboard =
  case keyboard of
    Normal -> SendMessage.ReplyKeyboardRemove
    Options opts ->
      SendMessage.InlineKeyboard
        (map (\o -> [o]) opts) -- display options as a column
        False
        True
