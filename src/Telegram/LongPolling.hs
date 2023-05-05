module Telegram.LongPolling (run) where

import Control.Concurrent (threadDelay)
import qualified Telegram
import qualified Telegram.Api as Api
import qualified Telegram.Api.GetUpdates as GetUpdates
import Telegram.Api.Update (Update)
import qualified Telegram.Api.Update as Update
import Telegram.Message (Message)
import qualified Telegram.Message as Message


data State = State
  { handler :: Telegram.Handler
  , fetchState :: FetchState
  }


data FetchState
  = Buffered
      Update
      [Update]
  | NeedMore (Maybe Integer)


type Callback = Message -> IO ()


run :: Callback -> Telegram.Handler -> IO ()
run callback handler = do
  let state = State {handler = handler, fetchState = NeedMore Nothing}
  loop callback state


loop :: Callback -> State -> IO ()
loop callback state = do
  (message, state) <- getMessage state
  callback message
  loop callback state


getMessage :: State -> IO (Message, State)
getMessage state =
  case fetchState state of
    Buffered nextUpdate rest ->
      return
        ( Message.fromUpdate nextUpdate
        , case rest of
            u : us -> state {fetchState = Buffered u us}
            [] -> state {fetchState = NeedMore $ Just $ Update.updateId nextUpdate}
        )
    NeedMore lastUpdateId -> do
      response <- requestUpdates (handler state) lastUpdateId
      case GetUpdates.result response of
        u : us -> do
          getMessage (state {fetchState = Buffered u us})
        [] -> do
          putStrLn "No updates found! Will retry in a bit"
          threadDelay (1 * 1000 * 1000)
          getMessage state


requestUpdates ::
  Telegram.Handler ->
  Maybe Integer ->
  IO GetUpdates.UpdateResponse
requestUpdates handler lastUpdateId = do
  result <- Telegram.getUpdates handler ((+ 1) <$> lastUpdateId)
  case result of
    (Left Api.GetUpdatesApiError) -> do
      putStrLn "Error contacting telegram for updates. Will retry soon."
      requestUpdates handler lastUpdateId
    (Left Api.GetUpdatesDecodingError) -> do
      putStrLn "Decoding error! Skipping message"
      requestUpdates handler ((+ 1) <$> lastUpdateId)
    (Right updates) -> return updates
