module Core (initialize, update, Model, Event(..), Effect(..)) where

import           Conversation.Parameters.Split (Split)
import qualified Conversation.Parameters.Split as Split
import           Core.Conversation (Conversation)
import qualified Settings
import           Settings (Settings)
import qualified Splitwise
import qualified Telegram
import           Telegram.Api (ChatId)
import           Telegram.Message (Message)
import qualified Telegram.Message as Message
import qualified Telegram.Username
import           Telegram.Username (Username)

data Model = Model
  { telegramToken :: Telegram.Token
  , splitwiseAuth :: Splitwise.Group
  , userA :: User
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

data Effect =
  LogError String

initialize :: Settings -> Model
initialize settings
  = let
      presetA = Settings.userASplitwisePreset settings
      presetB = 100 - presetA
    in
      Model
        { telegramToken = Telegram.Token $ Settings.telegramToken settings
        , splitwiseAuth = Splitwise.group
          (Settings.userASplitwiseToken settings)
          (Settings.userASplitwiseId settings)
          (Settings.userBSplitwiseId settings)
        , userA         = User
          { telegramId        = (Telegram.Username.fromString
                                  (Settings.userATelegramId settings)
                                )
          , splitwiseRole     = Splitwise.Owner
          , preset            = Split.init presetA
          , conversationState = Uninitialized
          }
        , userB         = User
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
          let identifiedUsers = case userId of
                UserA -> (getUser UserA model, getUser UserB model)
                UserB -> (getUser UserB model, getUser UserB model)
              (updatedCurrentUser, updatedPeer, effects) =
                answerMessage msg identifiedUsers
          in  ( (updateUser userId updatedCurrentUser)
              . (updateUser (otherUserId userId) updatedPeer)
              $ model
              , effects
              )

answerMessage :: Message -> (User, User) -> (User, User, [Effect])
answerMessage msg (currentUser, peer) =
  -- TODO!
  ( currentUser
  , peer
  , [ LogError
        (  "Received message: "
        ++ (Message.text msg)
        ++ " from "
        ++ (show (telegramId currentUser))
        )
    ]
  )

matchUserId :: Model -> Username -> Maybe UserId
matchUserId model username | (telegramId . userA) model == username = Just UserA
                           | (telegramId . userB) model == username = Just UserB
                           | otherwise                              = Nothing

otherUserId :: UserId -> UserId
otherUserId userId = case userId of
  UserA -> UserB
  UserB -> UserA

getUser :: UserId -> Model -> User
getUser userId = case userId of
  UserA -> userA
  UserB -> userB

updateUser :: UserId -> User -> Model -> Model
updateUser userId user model = case userId of
  UserA -> model { userA = user }
  UserB -> model { userB = user }
