module Telegram where

import Network.HTTP.Client (httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Telegram.Api
import Data.Aeson (eitherDecode)
import Control.Concurrent (threadDelay)
import Control.Arrow ((>>>))

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

data Update =
  Update
    { username :: String
    , text :: String
    } deriving Show

init :: Token -> State
init token =
  State
    { token = token
    , fetchState = NeedMore Nothing
    }

getUpdate :: State -> IO (Update, State)
getUpdate state =
  case fetchState state of
    Buffered nextUpdate rest ->
      return
        ( buildUpdate nextUpdate
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
            getUpdate $ state { fetchState = Buffered u us }

          [] ->
            do
              putStrLn "No updates found! Will retry in a bit"
              threadDelay (1 * 1000 * 1000)
              getUpdate state

buildUpdate :: Telegram.Api.Update -> Update
buildUpdate update =
  let
    message = Telegram.Api.message update
  in
  Update
    { username = (Telegram.Api.username . Telegram.Api.from) message
    , text = Telegram.Api.text message
    }

requestUpdates :: Token -> Maybe Int -> IO (Telegram.Api.UpdateResponse)
requestUpdates token lastUpdateId =
  do
    -- TODO: create the manager once and store it
    manager <- newTlsManager
    request <- parseRequest (updatesUrl token lastUpdateId)
    response <- httpLbs request manager
    case eitherDecode (responseBody response) of
      Left err -> do
        putStrLn "Decoding error! Skipping message"
        putStrLn err
        requestUpdates token lastUpdateId -- TODO: check this
      Right update ->
        return update

updatesUrl :: Token -> Maybe Int -> String
updatesUrl token lastUpdateId =
  concat
    [ "https://api.telegram.org/bot"
    , token
    , "/getUpdates"
    , queryString
      [ ("timeout", Just "10")
      , ("limit", Just "30")
      , ("allowed_updates", Just "%5B%22message%22%5D")
      , ("offset", ((+1) >>> show) <$> lastUpdateId
        )
      ]
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


sendMessage :: String -> String -> IO ()
sendMessage _ _ =
  return ()
