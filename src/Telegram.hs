module Telegram where

import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as ByteString
import Network.HTTP.Client
  ( Manager
  , Request
  , RequestBody(..)
  , httpLbs
  , method
  , parseRequest
  , parseRequest_
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  , setQueryString
  )
import Network.HTTP.Types.Status (statusCode)
import qualified Telegram.Api
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
  = Buffered Telegram.Api.Update
             [Telegram.Api.Update]
  | NeedMore (Maybe Int)

data Message = Message
  { chatId :: ChatId
  , username :: Username
  , text :: String
  }

data ChatId =
  ChatId Integer
  deriving (Show)

init :: Token -> Manager -> State
init token manager = State {token = token, manager = manager, fetchState = NeedMore Nothing}

getMessage :: State -> IO (Message, State)
getMessage state =
  case fetchState state of
    Buffered nextUpdate rest ->
      return
        ( toMessage nextUpdate
        , case rest of
            u:us -> state {fetchState = Buffered u us}
            [] -> state {fetchState = NeedMore $ Just $ Telegram.Api.updateId nextUpdate})
    NeedMore lastUpdateId -> do
      response <- requestUpdates (token state) (manager state) lastUpdateId
      case Telegram.Api.result response of
        u:us -> getMessage $ state {fetchState = Buffered u us}
        [] -> do
          putStrLn "No updates found! Will retry in a bit"
          threadDelay (1 * 1000 * 1000)
          getMessage state

toMessage :: Telegram.Api.Update -> Message
toMessage update =
  let message = Telegram.Api.message update
      user = Telegram.Api.from message
   in Message
        { chatId = ChatId (Telegram.Api.id user)
        , username = Username (Telegram.Api.username user)
        , text = Telegram.Api.text message
        }

requestUpdates :: Token -> Manager -> Maybe Int -> IO (Telegram.Api.UpdateResponse)
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
  let url = apiUrl token "getUpdates"
      setParams =
        setQueryString
          [ ("timeout", Just "10")
          , ("limit", Just "30")
          , ("allowed_updates", Just "%5B%22message%22%5D")
          , ("offset", ((+ 1) >>> show >>> ByteString.pack) <$> lastUpdateId)
          ]
   in setParams <$> parseRequest url

apiUrl :: Token -> String -> String
apiUrl token apiMethod = concat ["https://api.telegram.org/bot", token, "/", apiMethod]

sendMessage :: State -> ChatId -> String -> IO ()
sendMessage state chatId text = do
  request <- newSendRequest (token state) chatId text
  response <- httpLbs request (manager state)
  case (responseStatus >>> statusCode) response of
    200 -> return ()
    _ -> do
      putStrLn "Error sending message"
      return ()

newSendRequest :: Token -> ChatId -> String -> IO Request
newSendRequest token (ChatId chatId) text =
  let url = apiUrl token "sendMessage"
      message = SendMessage.Message chatId text Nothing
   in return $
      (parseRequest_ url)
        { method = "POST"
        , requestHeaders = [("Content-Type", "application/json")]
        , requestBody = RequestBodyLBS (encode message)
        }
