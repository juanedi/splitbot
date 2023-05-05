module Core (
  Core.init,
  update,
  UserId (..),
  Model,
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
import qualified Telegram
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


init :: LocalStore.Handler -> Settings -> IO Model
init localStore settings = do
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


update ::
  Telegram.Handler ->
  Splitwise.Handler ->
  LocalStore.Handler ->
  Message ->
  Model ->
  IO Model
update telegram splitwise localStore msg model =
  let username = Message.username msg
   in case matchUserId model username of
        Nothing -> do
          putStrLn $ "Ignoring message from unknown user: " ++ show username
          pure model
        Just userId -> do
          let currentUser = getUser userId model
              chatId = Message.chatId msg

          updatedCurrentUser <-
            answerMessage
              telegram
              splitwise
              (getPeerChatId userId model)
              msg
              currentUser

          when (shouldStoreChatId chatId currentUser) $
            writeChatId localStore userId chatId

          pure (updateUser userId updatedCurrentUser model)


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


answerMessage ::
  Telegram.Handler ->
  Splitwise.Handler ->
  Maybe ChatId ->
  Message ->
  User ->
  IO User
answerMessage telegram splitwise peerChatId msg currentUser = do
  let ownChatId = Message.chatId msg
      txt = Message.text msg
  maybeConversation <-
    case conversationState currentUser of
      Uninitialized ->
        Just
          <$> Conversation.start
            telegram
            ownChatId
            txt
            (preset currentUser)
      Inactive _ ->
        Just
          <$> Conversation.start
            telegram
            ownChatId
            txt
            (preset currentUser)
      Active _ conversation ->
        Conversation.messageReceived
          telegram
          splitwise
          ownChatId
          peerChatId
          (splitwiseRole currentUser)
          txt
          conversation
  pure
    ( currentUser
        { conversationState =
            case maybeConversation of
              Nothing -> Inactive ownChatId
              Just c -> Active ownChatId c
        }
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
