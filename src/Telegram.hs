module Telegram ( sendMessage) where

import qualified Network.HTTP.Client as Http
import           Telegram.Api ( Token , ChatId(..))
import qualified Telegram.Api as Api
import           Telegram.Reply (Reply)

sendMessage :: Http.Manager -> Token -> ChatId -> Reply -> IO ()
sendMessage http token chatId reply =
  -- TODO: handle error
  Api.sendMessage token http chatId reply >> return ()
