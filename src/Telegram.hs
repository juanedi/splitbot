module Telegram
  ( Telegram.init
  , getMessage
  , sendMessage
  , toMessage
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
import           Telegram.Api.Update (Update)
import qualified Telegram.Api.Update as Update
import           Telegram.Reply (Reply)

data State
  = Buffered Update
             [Update]
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

init :: State
init = NeedMore Nothing

getMessage :: Http.Manager -> Token -> State -> IO (Message, State)
getMessage http token state = case state of
  Buffered nextUpdate rest -> return
    ( toMessage nextUpdate
    , case rest of
      u : us -> (Buffered u us)
      []     -> (NeedMore $ Just $ Update.updateId nextUpdate)
    )
  NeedMore lastUpdateId -> do
    response <- requestUpdates token http lastUpdateId
    case GetUpdates.result response of
      u : us -> do
        getMessage http token (Buffered u us)
      [] -> do
        putStrLn "No updates found! Will retry in a bit"
        threadDelay (1 * 1000 * 1000)
        getMessage http token state

toMessage :: Update -> Message
toMessage update =
  let message = Update.message update
      user    = Update.from message
  in  Message
        { chatId   = ChatId (Update.id user)
        , username = Username (Update.username user)
        , text     = Update.text message
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

sendMessage :: Http.Manager -> Token -> ChatId -> Reply -> IO ()
sendMessage http token chatId reply =
  -- TODO: handle error
  Api.sendMessage token http chatId reply >> return ()
