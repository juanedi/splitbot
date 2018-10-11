module Telegram.Api (
  getUpdates,
  sendMessage,
  ChatId(..),
  Reply(..),
  ReplyKeyboard(..),
  Token,
  GetUpdatesResult,
  GetUpdatesError(..)
  ) where

import Control.Arrow ((>>>))
import Data.Aeson (eitherDecode, encode)
import Network.HTTP.Client
  ( RequestBody(..)
  , httpLbs
  , method
  , parseRequest_
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  )
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types.Status (statusCode)
import qualified Telegram.Api.GetUpdates as GetUpdates
import qualified Telegram.Api.SendMessage as SendMessage
import Control.Exception as Exception
import Data.ByteString.Lazy (ByteString)

type Token = String

data Reply =
  Reply String
        ReplyKeyboard

data ReplyKeyboard
  = Normal
  | Options [String]

data ChatId =
  ChatId Integer
  deriving (Show)

type GetUpdatesResult = Either GetUpdatesError GetUpdates.UpdateResponse

data GetUpdatesError
  = GetUpdatesApiError
  | GetUpdatesDecodingError

type SendMessageResult
  = Bool


getUpdates :: Token -> Http.Manager -> Maybe Integer -> IO GetUpdatesResult
getUpdates token manager offset = do
  request <- newUpdateRequest token offset
  result  <- runRequest request manager
  case result of
    Left  _err     -> return (Left GetUpdatesApiError)
    Right response -> case (statusCode . responseStatus) response of
      200 -> case eitherDecode (responseBody response) of
        Left _ -> do
          return (Left GetUpdatesDecodingError)
        Right update -> do
          return (Right update)
      _ -> do
        return (Left GetUpdatesApiError)

runRequest
  :: Http.Request
  -> Http.Manager
  -> IO (Either Http.HttpException (Http.Response ByteString))
runRequest request manager = Exception.try (Http.httpLbs request manager)

newUpdateRequest :: Token -> Maybe Integer -> IO Http.Request
newUpdateRequest token offset =
  return $ (parseRequest_ (apiUrl token "getUpdates"))
    { method         = "GET"
    , requestHeaders = [("Content-Type", "application/json")]
    , requestBody    = RequestBodyLBS (encode updateRequest)
    }
 where
  updateRequest = GetUpdates.Request
    { GetUpdates.timeout        = 10
    , GetUpdates.limit          = 30
    , GetUpdates.allowedUpdates = ["message"]
    , GetUpdates.offset         = offset
    }

apiUrl :: Token -> String -> String
apiUrl token apiMethod =
  concat ["https://api.telegram.org/bot", token, "/", apiMethod]

sendMessage :: Token -> Http.Manager -> ChatId -> Reply -> IO SendMessageResult
sendMessage token manager chatId reply = do
  request  <- newSendRequest token chatId reply
  response <- httpLbs request manager
  let status = (responseStatus >>> statusCode) response
  return (status == 200)

newSendRequest :: Token -> ChatId -> Reply -> IO Http.Request
newSendRequest token (ChatId chatId) (Reply text keyboard) =
  return $ (parseRequest_ (apiUrl token "sendMessage"))
    { method         = "POST"
    , requestHeaders = [("Content-Type", "application/json")]
    , requestBody    = (RequestBodyLBS . encode) message
    }
  where message = SendMessage.Message chatId text (replyMarkup keyboard)

replyMarkup :: ReplyKeyboard -> SendMessage.ReplyMarkup
replyMarkup keyboard = case keyboard of
  Normal -> SendMessage.ReplyKeyboardRemove
  Options opts ->
    SendMessage.InlineKeyboard (map (\o -> [o]) opts) -- display options as a column
                                                      False True
