module Telegram
  ( Telegram.init
  , getMessage
  , sendMessage
  , username
  , chatId
  , text
  , Message
  , State
  , Username(..)
  ) where

import           Control.Concurrent (threadDelay)
import qualified Network.HTTP.Client as Http
import           Telegram.Api ( Token , ChatId(..))
import qualified Telegram.Api as Api
import qualified Telegram.Api.GetUpdates as GetUpdates
import           Telegram.Reply (Reply)

data State = State
  { token :: Token
  , fetchState :: FetchState
  }

data FetchState
  = Buffered GetUpdates.Update
             [GetUpdates.Update]
  | NeedMore (Maybe Integer)

data Username =
  Username String

instance Eq Username where
  (==) (Username a) (Username b) = a == b

instance Show Username where
  show (Username a) = a

data Message = Message
  { chatId :: ChatId
  , username :: Username
  , text :: String
  }

init :: Token -> State
init token = State {token = token, fetchState = NeedMore Nothing}

getMessage :: Http.Manager -> State -> IO (Message, State)
getMessage http state = case fetchState state of
  Buffered nextUpdate rest -> return
    ( toMessage nextUpdate
    , case rest of
      u : us -> state { fetchState = Buffered u us }
      [] ->
        state { fetchState = NeedMore $ Just $ GetUpdates.updateId nextUpdate }
    )
  NeedMore lastUpdateId -> do
    response <- requestUpdates (token state) http lastUpdateId
    case GetUpdates.result response of
      u : us -> do
        getMessage http $ state { fetchState = Buffered u us }
      [] -> do
        putStrLn "No updates found! Will retry in a bit"
        threadDelay (1 * 1000 * 1000)
        getMessage http state

toMessage :: GetUpdates.Update -> Message
toMessage update =
  let message = GetUpdates.message update
      user    = GetUpdates.from message
  in  Message
        { chatId   = ChatId (GetUpdates.id user)
        , username = Username (GetUpdates.username user)
        , text     = GetUpdates.text message
        }

requestUpdates
  :: Token -> Http.Manager -> Maybe Integer -> IO (GetUpdates.UpdateResponse)
requestUpdates token manager lastUpdateId = do
  result <- Api.getUpdates token manager ((+ 1) <$> lastUpdateId)
  case result of
    (Left Api.GetUpdatesApiError) -> do
      putStrLn "Error contacting telegram for updates. Will retry soon."
      requestUpdates token manager lastUpdateId
    (Left Api.GetUpdatesDecodingError) -> do
      putStrLn "Decoding error! Skipping message"
      requestUpdates token manager ((+ 1) <$> lastUpdateId)
    (Right updates) -> return updates

sendMessage :: Http.Manager -> State -> ChatId -> Reply -> IO ()
sendMessage http state chatId reply =
  -- TODO: handle error
  Api.sendMessage (token state) http chatId reply >> return ()
