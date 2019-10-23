module Core
  ( initialize
  , update
  , ContactInfo(..)
  , Model
  , Event(..)
  , Effect(..)
  ) where

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
  | ConversationEvent UserId Conversation.Event

data Effect
  = LogError String
  | ConversationEffect ContactInfo Conversation.Effect

data ContactInfo = ContactInfo
  { ownUserId :: UserId
  , ownChatId :: ChatId
  , ownRole :: Splitwise.Role
  , peerChatId :: Maybe ChatId
  }

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
  ConversationEvent _userId _event ->
    -- TODO
    (model, [])

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
          let (currentUser, peer) = case userId of
                UserA -> (getUser UserA model, getUser UserB model)
                UserB -> (getUser UserB model, getUser UserA model)

              contactInfo =
                (ContactInfo
                  { ownUserId  = userId
                  , ownChatId  = (Message.chatId msg)
                  , ownRole    = (splitwiseRole currentUser)
                  , peerChatId = case conversationState peer of
                    Uninitialized   -> Nothing
                    Inactive chatId -> Just chatId
                    Active chatId _ -> Just chatId
                  }
                )


              (updatedCurrentUser, effects) =
                answerMessage msg contactInfo currentUser
          in  ((updateUser userId updatedCurrentUser) model, effects)

answerMessage :: Message -> ContactInfo -> User -> (User, [Effect])
answerMessage msg contactInfo currentUser
  = let
      txt                          = Message.text msg
      (maybeConversation, effects) = case conversationState currentUser of
        Uninitialized -> (Conversation.start txt (preset currentUser))
        Inactive _    -> (Conversation.start txt (preset currentUser))
        Active _ conversation ->
          (Conversation.messageReceived txt conversation)
      userChatId = (Message.chatId msg)
    in
      ( currentUser
        { conversationState = case maybeConversation of
                                Nothing -> Inactive userChatId
                                Just c  -> Active userChatId c
        }
      , fmap (ConversationEffect contactInfo) effects
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
