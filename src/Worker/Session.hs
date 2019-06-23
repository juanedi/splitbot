module Worker.Session
  (Session(..)
  , load
  , sync
  )
  where

import           Conversation (Conversation)
import           Telegram.Api (ChatId)
import           Telegram.Message (Message)
import qualified Telegram.Message as Message
import           Telegram.Username (Username)
import           Worker.Model ( UserId(..)
                              , ConversationState(..)
                              , User
                              , Model
                              , identity
                              , userA
                              , userB
                              , telegramId
                              , conversationState
                              )
import qualified Worker.Model as Model

data Session = Session
  { user :: User
  , userId :: UserId
  , chatId :: ChatId
  , conversation :: Maybe Conversation
  , buddyChatId :: Maybe ChatId
  }

load :: Model -> Message -> Maybe Session
load state message =
  let username    = Message.username message
      maybeUserId = matchUserId state username
  in  case maybeUserId of
        Nothing -> Nothing
        Just userId ->
          let user  = getUser userId state
              buddy = getUser (otherUserId userId) state
          in  Just
                (Session
                  { user         = user
                  , userId       = userId
                  , chatId       = Message.chatId message
                  , conversation = case conversationState user of
                    Nothing                      -> Nothing
                    Just (Inactive _           ) -> Nothing
                    Just (Active _ conversation) -> Just (conversation)
                  , buddyChatId  = case conversationState buddy of
                    Nothing                -> Nothing
                    Just (Inactive chatId) -> Just chatId
                    Just (Active chatId _) -> Just chatId
                  }
                )

sync :: Session -> Model -> Model
sync session model =
  let updatedUser = (user session)
        { conversationState = Just $ case conversation session of
                                Nothing   -> Inactive (chatId session)
                                Just conv -> Active (chatId session) conv
        }
  in  Model.updateUser (userId session) updatedUser model

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
