module Telegram (
  Handler,
  Token (..),
  Telegram.init,
  getUpdates,
  sendMessage,
) where

import qualified Network.HTTP.Client as Http
import Telegram.Api (ChatId (..), GetUpdatesResult)
import qualified Telegram.Api as Api
import Telegram.Reply (Reply)


data Handler = Handler
  { http :: Http.Manager
  , token :: Token
  }


newtype Token = Token String


init :: Http.Manager -> Token -> Handler
init http_ token_ =
  Handler {http = http_, token = token_}


getUpdates :: Handler -> Maybe Integer -> IO GetUpdatesResult
getUpdates (Handler http (Token token)) = Api.getUpdates token http


sendMessage :: Handler -> ChatId -> Reply -> IO Bool
sendMessage (Handler http (Token token)) = Api.sendMessage token http
