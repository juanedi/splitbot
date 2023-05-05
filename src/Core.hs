module Core (
  initialize,
  update,
  UserId (..),
  ContactInfo (..),
  Model,
  Event (..),
  Effect (..),
) where

import Control.Monad (when)
import Conversation (Conversation)
import qualified Conversation
import Conversation.Expense (Split (..))
import qualified LocalStore
import Settings (Settings)
import qualified Settings
import qualified Splitwise
import qualified System.FilePath.Posix as FilePath
import Telegram.Api (ChatId (..))
import Telegram.Message (Message)
import qualified Telegram.Message as Message
import Telegram.Username (Username)
import qualified Telegram.Username
import Text.Read (readMaybe)


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
  deriving (Show)


data Event
  = MessageReceived Message
  | ConversationEvent UserId Conversation.Event
  deriving (Show)


data Effect
  = LogError String
  | ConversationEffect ContactInfo Conversation.Effect


data ContactInfo = ContactInfo
  { ownUserId :: UserId
  , ownChatId :: ChatId
  , ownRole :: Splitwise.Role
  , peerChatId :: Maybe ChatId
  }


initialize :: LocalStore.Handler -> Settings -> IO Model
initialize localStore settings = do
  let presetA = Settings.userASplitwisePreset settings
      presetB = 100 - presetA
  chatIdA <- readChatId localStore Core.UserA
  chatIdB <- readChatId localStore Core.UserB
  pure
    ( Model
        { userA =
            User
              { telegramId = Telegram.Username.fromString (Settings.userATelegramId settings)
              , splitwiseRole = Splitwise.Owner
              , preset = Split presetA
              , conversationState = maybe Uninitialized Inactive chatIdA
              }
        , userB =
            User
              { telegramId = Telegram.Username.fromString (Settings.userBTelegramId settings)
              , splitwiseRole = Splitwise.Peer
              , preset = Split presetB
              , conversationState = maybe Uninitialized Inactive chatIdB
              }
        }
    )


update :: LocalStore.Handler -> Event -> Model -> IO (Model, [Effect])
update localStore event model =
  case event of
    MessageReceived msg ->
      --
      updateFromMessage localStore msg model
    ConversationEvent userId conversationEvent ->
      let (updatedUser, effects) =
            relayEvent
              userId
              (getUser userId model)
              (getPeerChatId userId model)
              conversationEvent
       in pure (updateUser userId updatedUser model, effects)


relayEvent :: UserId -> User -> Maybe ChatId -> Conversation.Event -> (User, [Effect])
relayEvent userId currentUser peerChatId event =
  case conversationState currentUser of
    Uninitialized -> (currentUser, [])
    Inactive _ -> (currentUser, [])
    Active chatId conversation ->
      let (updatedConversation, conversationEffects) =
            Conversation.update event conversation

          contactInfo =
            ( ContactInfo
                { ownUserId = userId
                , ownChatId = chatId
                , ownRole = splitwiseRole currentUser
                , peerChatId = peerChatId
                }
            )
       in ( currentUser
              { conversationState =
                  case updatedConversation of
                    Nothing -> Inactive chatId
                    Just conv -> Active chatId conv
              }
          , fmap (ConversationEffect contactInfo) conversationEffects
          )


readChatId :: LocalStore.Handler -> UserId -> IO (Maybe ChatId)
readChatId localStore userId =
  do
    readResult <- LocalStore.read localStore (chatIdPath userId)
    return
      ( case readResult of
          Left _err -> Nothing
          Right contents -> fmap ChatId (readMaybe contents)
      )


writeChatId :: LocalStore.Handler -> UserId -> ChatId -> IO ()
writeChatId localStore userId (ChatId chatId) =
  LocalStore.write localStore (chatIdPath userId) (show chatId)


chatIdPath :: UserId -> FilePath
chatIdPath userId =
  FilePath.joinPath
    [ "users"
    , case userId of
        Core.UserA -> "a"
        Core.UserB -> "b"
    , "chatId"
    ]


updateFromMessage :: LocalStore.Handler -> Message -> Model -> IO (Model, [Effect])
updateFromMessage localStore msg model =
  let username = Message.username msg
   in case matchUserId model username of
        Nothing ->
          pure
            ( model
            , [LogError $ "Ignoring message from unknown user: " ++ show username]
            )
        Just userId -> do
          let currentUser = getUser userId model

              chatId = Message.chatId msg

              contactInfo =
                ( ContactInfo
                    { ownUserId = userId
                    , ownChatId = chatId
                    , ownRole = splitwiseRole currentUser
                    , peerChatId = getPeerChatId userId model
                    }
                )

              (updatedCurrentUser, effects) =
                answerMessage msg contactInfo currentUser

          when (shouldStoreChatId chatId currentUser) $
            writeChatId localStore userId chatId

          pure (updateUser userId updatedCurrentUser model, effects)


answerMessage :: Message -> ContactInfo -> User -> (User, [Effect])
answerMessage msg contactInfo currentUser =
  let txt = Message.text msg
      (maybeConversation, effects) =
        case conversationState currentUser of
          Uninitialized -> Conversation.start txt (preset currentUser)
          Inactive _ -> Conversation.start txt (preset currentUser)
          Active _ conversation -> Conversation.messageReceived txt conversation
      userChatId = Message.chatId msg
   in ( currentUser
          { conversationState =
              case maybeConversation of
                Nothing -> Inactive userChatId
                Just c -> Active userChatId c
          }
      , fmap (ConversationEffect contactInfo) effects
      )


shouldStoreChatId :: ChatId -> User -> Bool
shouldStoreChatId chatId user =
  case conversationState user of
    Uninitialized -> True
    Inactive chatId_ -> chatId /= chatId_
    Active chatId_ _ -> chatId /= chatId_


matchUserId :: Model -> Username -> Maybe UserId
matchUserId model username
  | (telegramId . userA) model == username = Just UserA
  | (telegramId . userB) model == username = Just UserB
  | otherwise = Nothing


getUser :: UserId -> Model -> User
getUser userId =
  case userId of
    UserA -> userA
    UserB -> userB


getPeer :: UserId -> Model -> User
getPeer userId =
  case userId of
    UserA -> userB
    UserB -> userA


updateUser :: UserId -> User -> Model -> Model
updateUser userId user model =
  case userId of
    UserA -> model {userA = user}
    UserB -> model {userB = user}


getPeerChatId :: UserId -> Model -> Maybe ChatId
getPeerChatId userId model =
  case conversationState (getPeer userId model) of
    Uninitialized -> Nothing
    Inactive chatId -> Just chatId
    Active chatId _ -> Just chatId
