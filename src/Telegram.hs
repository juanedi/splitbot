module Telegram (Token(..), getUpdates, sendMessage) where

import qualified Network.HTTP.Client as Http
import           Telegram.Api (ChatId(..), GetUpdatesResult)
import qualified Telegram.Api as Api
import           Telegram.Reply (Reply)

data Token = Token String

getUpdates :: Http.Manager -> Token -> Maybe Integer -> IO GetUpdatesResult
getUpdates http (Token token) offset = Api.getUpdates token http offset

sendMessage :: Http.Manager -> Token -> ChatId -> Reply -> IO ()
sendMessage http (Token token) chatId reply =
  -- TODO: handle error
  Api.sendMessage token http chatId reply >> return ()
