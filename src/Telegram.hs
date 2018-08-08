module Telegram where

import Network.HTTP.Client (httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Telegram.Api
import Data.Aeson (eitherDecode)
import Control.Concurrent (threadDelay)

data State =
  State
    { token :: Token
    , fetchState :: FetchState
    } deriving Show

type Token = String

data FetchState
  = Initial
  -- | Buffered [Update]
  | NeedMore Int
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
    , fetchState = Initial
    }

getUpdate :: State -> IO (Update, State)
getUpdate state =
  -- TODO: request a longer list of messages and use that as a local buffer
  do
    response <- requestUpdates state
    case Telegram.Api.result response of
      update : _ ->
        return $
          ( buildUpdate update
          , state { fetchState = NeedMore $ Telegram.Api.updateId update }
          )

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

requestUpdates :: State -> IO (Telegram.Api.UpdateResponse)
requestUpdates state =
  do
    -- TODO: create the manager once and store it
    manager <- newTlsManager
    request <- parseRequest (updatesUrl (token state) (fetchState state))
    response <- httpLbs request manager
    case eitherDecode (responseBody response) of
      Left err -> do
        putStrLn "Decoding error! Skipping message"
        putStrLn err
        requestUpdates state
      Right update ->
        return update

updatesUrl :: Token -> FetchState -> String
updatesUrl token fetchState =
  concat
    [ "https://api.telegram.org/bot"
    , token
    , "/getUpdates"
    , queryString
      [ ("timeout", Just "10")
      , ("limit", Just "30")
      , ("offset",
         case fetchState of
           Initial ->
             Nothing
           NeedMore lastUpdateId ->
             Just (show (lastUpdateId + 1))
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
