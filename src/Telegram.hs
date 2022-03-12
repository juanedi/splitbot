module Telegram (Token (..), getUpdates, sendMessage) where

import qualified Network.HTTP.Client as Http
import Telegram.Api (ChatId (..), GetUpdatesResult)
import qualified Telegram.Api as Api
import Telegram.Reply (Reply)


newtype Token = Token String


getUpdates :: Http.Manager -> Token -> Maybe Integer -> IO GetUpdatesResult
getUpdates http (Token token) = Api.getUpdates token http


sendMessage :: Http.Manager -> Token -> ChatId -> Reply -> IO Bool
sendMessage http (Token token) = Api.sendMessage token http
