module Worker.Session
  (Session(..)
  , load
  )
  where

import           Telegram.Api (ChatId)
import           Telegram.Message (Message)
import qualified Telegram.Message as Message
import           Telegram.Username (Username)
import           Worker.Model

data Session = Session
  { userId :: UserId
  , chatId :: ChatId
  , user :: User
  , buddy :: UserIdentity
  }

load :: Model -> Message -> Maybe Session
load state message =
  let username    = Message.username message
      maybeUserId = matchUserId state username
  in  case maybeUserId of
        Nothing     -> Nothing
        Just userId -> Just
          (Session
            { userId = userId
            , chatId = Message.chatId message
            , user   = getUser userId state
            , buddy  = identity (getUser (otherUserId userId) state)
            }
          )

getUser :: UserId -> Model -> User
getUser userId = case userId of
  UserA -> userA
  UserB -> userB

otherUserId :: UserId -> UserId
otherUserId userId = case userId of
  UserA -> UserB
  UserB -> UserA

matchUserId :: Model -> Username -> Maybe UserId
matchUserId state uname
  | (telegramId . identity . userA) state == uname = Just UserA
  | (telegramId . identity . userB) state == uname = Just UserB
  | otherwise = Nothing
