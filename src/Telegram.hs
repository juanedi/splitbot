module Telegram where

import Network.HTTP.Client (Request, httpLbs, parseRequest, responseBody, method, responseStatus, setQueryString)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Telegram.Api
import Data.Aeson (eitherDecode)
import Control.Concurrent (threadDelay)
import Control.Arrow ((>>>))
import qualified Data.ByteString.Char8 as ByteString

data State =
  State
    { token :: Token
    , fetchState :: FetchState
    } deriving Show

type Token = String

data FetchState
  = Buffered Telegram.Api.Update [Telegram.Api.Update]
  | NeedMore (Maybe Int)
    deriving Show

data Message =
  Message
    { chatId :: ChatId
    , username :: String
    , text :: String
    } deriving Show

data ChatId = ChatId Integer deriving Show

init :: Token -> State
init token =
  State
    { token = token
    , fetchState = NeedMore Nothing
    }

getMessage :: State -> IO (Message, State)
getMessage state =
  case fetchState state of
    Buffered nextUpdate rest ->
      return
        ( toMessage nextUpdate
        , case rest of
            u : us ->
              state { fetchState = Buffered u us }
            [] ->
              state { fetchState = NeedMore $ Just $ Telegram.Api.updateId nextUpdate }
        )


    NeedMore lastUpdateId ->
      do
        response <- requestUpdates (token state) lastUpdateId
        case Telegram.Api.result response of
          u : us ->
            getMessage $ state { fetchState = Buffered u us }

          [] ->
            do
              putStrLn "No updates found! Will retry in a bit"
              threadDelay (1 * 1000 * 1000)
              getMessage state

toMessage :: Telegram.Api.Update -> Message
toMessage update =
  let
    message = Telegram.Api.message update
    user = Telegram.Api.from message
  in
  Message
    { chatId = ChatId $ Telegram.Api.id user
    , username = Telegram.Api.username user
    , text = Telegram.Api.text message
    }

requestUpdates :: Token -> Maybe Int -> IO (Telegram.Api.UpdateResponse)
requestUpdates token lastUpdateId =
  do
    -- TODO: create the manager once and store it
    manager <- newTlsManager
    request <- newUpdateRequest token lastUpdateId
    response <- httpLbs request manager
    case eitherDecode (responseBody response) of
      Left err -> do
        putStrLn "Decoding error! Skipping message"
        putStrLn err
        requestUpdates token lastUpdateId -- TODO: check this
      Right update ->
        return update

newUpdateRequest :: Token -> Maybe Int -> IO Request
newUpdateRequest token lastUpdateId =
  let
    url =
      apiUrl token "getUpdates"

    setParams =
      setQueryString
        [ ("timeout", Just "10")
        , ("limit", Just "30")
        , ("allowed_updates", Just "%5B%22message%22%5D")
        , ("offset", ((+1) >>> show >>> ByteString.pack) <$> lastUpdateId)
        ]
  in
    setParams <$> parseRequest url


apiUrl :: Token -> String -> String
apiUrl token apiMethod =
    concat
      [ "https://api.telegram.org/bot"
      , token
      , "/"
      , apiMethod
      ]

queryString :: [(String, Maybe String)] -> String
queryString =
  -- TODO: use a buffered string representation
  -- TODO: assuming values are already URL encoded
  foldl
  (\query (name, value) ->
     case value of
       Nothing -> query
       Just v  -> concat [query, "&", name, "=", v ]
     )
  "?"


sendMessage :: State -> ChatId -> String -> IO ()
sendMessage state chatId text =
  do
    -- TODO: create the manager once and store it
    -- TODO: error handling
    manager <- newTlsManager
    request <- newSendRequest (token state) chatId text
    _ <- httpLbs request manager
    return ()

newSendRequest :: Token -> ChatId -> String -> IO Request
newSendRequest token (ChatId chatId) text =
  let
    url =
      apiUrl token "sendMessage"

    initRequest url =
      fmap (\r -> r { method = "POST" }) (parseRequest url)

    setParams =
      -- TODO: use ByteString where applicable
      setQueryString
        [ ("chat_id", Just (ByteString.pack (show chatId)))
        , ("text", Just (ByteString.pack text))
        ]
  in
    setParams <$> initRequest url
