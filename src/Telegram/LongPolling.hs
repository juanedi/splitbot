module Telegram.LongPolling (run) where

import           Control.Concurrent (threadDelay)
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Client.TLS (newTlsManager)
import qualified Queue
import           Queue (Queue)
import qualified Telegram as Telegram
import qualified Telegram.Api as Api
import qualified Telegram.Api.GetUpdates as GetUpdates
import           Telegram.Api.Update (Update)
import qualified Telegram.Api.Update as Update
import           Telegram.Message (Message)
import qualified Telegram.Message as Message

data State = State
  { http :: Http.Manager
  , token :: Telegram.Token
  , fetchState :: FetchState
  }

data FetchState
  = Buffered Update
             [Update]
  | NeedMore (Maybe Integer)

run :: Queue Message -> Telegram.Token -> IO ()
run queue token = do
  http <- newTlsManager
  let state = State {http = http, token = token, fetchState = NeedMore Nothing}
  loop queue state

loop :: Queue Message -> State -> IO ()
loop queue state = do
  (message, state) <- getMessage state
  Queue.enqueue queue message
  loop queue state

getMessage :: State -> IO (Message, State)
getMessage state = case fetchState state of
  Buffered nextUpdate rest -> return
    ( Message.fromUpdate nextUpdate
    , case rest of
      u : us -> state { fetchState = Buffered u us }
      [] -> state { fetchState = NeedMore $ Just $ Update.updateId nextUpdate }
    )
  NeedMore lastUpdateId -> do
    response <- requestUpdates (token state) (http state) lastUpdateId
    case GetUpdates.result response of
      u : us -> do
        getMessage (state { fetchState = Buffered u us })
      [] -> do
        putStrLn "No updates found! Will retry in a bit"
        threadDelay (1 * 1000 * 1000)
        getMessage state

requestUpdates
  :: Telegram.Token
  -> Http.Manager
  -> Maybe Integer
  -> IO (GetUpdates.UpdateResponse)
requestUpdates token manager lastUpdateId = do
  result <- Telegram.getUpdates manager token ((+ 1) <$> lastUpdateId)
  case result of
    (Left Api.GetUpdatesApiError) -> do
      putStrLn "Error contacting telegram for updates. Will retry soon."
      requestUpdates token manager lastUpdateId
    (Left Api.GetUpdatesDecodingError) -> do
      putStrLn "Decoding error! Skipping message"
      requestUpdates token manager ((+ 1) <$> lastUpdateId)
    (Right updates) -> return updates
