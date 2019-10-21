module Core (initialize, update, Model, Event(..), Effect(..)) where

import qualified Conversation
import           Conversation (Conversation)
import           Conversation.Parameters.Split (Split)
import qualified Conversation.Parameters.Split as Split
import qualified Settings
import           Settings (Settings)
import qualified Splitwise
import           Telegram.Api (ChatId)
import           Telegram.Message (Message)
import qualified Telegram.Message as Message
import qualified Telegram.Username
import           Telegram.Username (Username)

data Model = Model
  { userA :: User
  , userB :: User
  }

data User = User
  { telegramId :: Username
  , splitwiseRole :: Splitwise.Role
  , preset :: Split
  , conversationState :: ConversationState
  }

data ConversationState
  = -- we haven't contacted this user yet, so we don't know their chat id
    Uninitialized
   | -- we have contacted the user before, and there is no active conversation
    Inactive ChatId
  | -- we are waiting for a reply from the user
    Active ChatId Conversation

data UserId
  = UserA
  | UserB

data Event
  = MessageReceived Message

data Effect = LogError String | ConversationEffect Conversation.Effect

initialize :: Settings -> Model
initialize settings =
  let presetA = Settings.userASplitwisePreset settings
      presetB = 100 - presetA
  in  Model
        { userA = User
          { telegramId        = (Telegram.Username.fromString
                                  (Settings.userATelegramId settings)
                                )
          , splitwiseRole     = Splitwise.Owner
          , preset            = Split.init presetA
          , conversationState = Uninitialized
          }
        , userB = User
          { telegramId        = (Telegram.Username.fromString
                                  (Settings.userBTelegramId settings)
                                )
          , splitwiseRole     = Splitwise.Peer
          , preset            = Split.init presetB
          , conversationState = Uninitialized
          }
        }


update :: Event -> Model -> (Model, [Effect])
update event model = case event of
  MessageReceived msg -> updateFromMessage msg model

updateFromMessage :: Message -> Model -> (Model, [Effect])
updateFromMessage msg model =
  let username = Message.username msg
  in  case matchUserId model username of
        Nothing ->
          ( model
          , [ LogError
                $  "Ignoring message from unknown user: "
                ++ (show username)
            ]
          )
        Just userId ->
          let currentUser = case userId of
                UserA -> (getUser UserA model)
                UserB -> (getUser UserB model)
              (updatedCurrentUser, effects) = answerMessage msg currentUser
          in  ((updateUser userId updatedCurrentUser) model, effects)

answerMessage :: Message -> User -> (User, [Effect])
answerMessage msg currentUser =
  let txt                          = Message.text msg
      (maybeConversation, effects) = case conversationState currentUser of
        Uninitialized         -> (Conversation.start txt (preset currentUser))
        Inactive _            -> (Conversation.start txt (preset currentUser))
        Active _ conversation -> (Conversation.advance txt conversation)
  in  ( currentUser
        { conversationState = case maybeConversation of
                                Nothing -> Inactive (Message.chatId msg)
                                Just c  -> Active (Message.chatId msg) c
        }
      , ConversationEffect <$> effects
      )

matchUserId :: Model -> Username -> Maybe UserId
matchUserId model username | (telegramId . userA) model == username = Just UserA
                           | (telegramId . userB) model == username = Just UserB
                           | otherwise                              = Nothing

getUser :: UserId -> Model -> User
getUser userId = case userId of
  UserA -> userA
  UserB -> userB

updateUser :: UserId -> User -> Model -> Model
updateUser userId user model = case userId of
  UserA -> model { userA = user }
  UserB -> model { userB = user }
