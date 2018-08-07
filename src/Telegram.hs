module Telegram where

import Network.HTTP.Client (httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Telegram.Api
import Data.Aeson (eitherDecode)
import Control.Concurrent (threadDelay)

data State =
  State
    { token :: Token
    , lastUpdate :: Maybe Int
    } deriving Show

type Token = String

data Update =
  Update
    { username :: String
    , text :: String
    } deriving Show

init :: Token -> State
init token =
  State
    { token = token
    , lastUpdate = Nothing
    }

getUpdate :: State -> IO (Update, State)
getUpdate state =
  -- TODO: request a longer list of messages and use that as a local buffer
  do
    response <- requestUpdates (token state) (lastUpdate state)
    case Telegram.Api.result response of
      update : _ ->
        return $
          ( buildUpdate update
          , state { lastUpdate = Just $ Telegram.Api.updateId update }
          )

      [] ->
        do
          putStrLn "No updates found! Will retry in 2 seconds"
          threadDelay (2 * 1000 * 1000)
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
requestUpdates token lastUpdate =
  do
    -- TODO: create the manager once and store it
    manager <- newTlsManager
    request <- parseRequest (updatesUrl token lastUpdate)
    response <- httpLbs request manager
    case eitherDecode (responseBody response) of
      Left err -> do
        putStrLn "Decoding error! Skipping message"
        putStrLn err
        requestUpdates token lastUpdate
      Right update ->
        return update

updatesUrl :: Token -> Maybe Int -> String
updatesUrl token lastUpdate =
  concat
    [ "https://api.telegram.org/bot"
    , token
    , "/getUpdates"
    , "?limit=1"
    , case lastUpdate of
        Nothing -> ""
        Just id -> "&offset=" ++ (show (id + 1))
    ]


sendMessage :: String -> String -> IO ()
sendMessage _ _ =
  return ()
