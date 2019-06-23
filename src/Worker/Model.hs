module Worker.Model
  ( Model(..)
  , User(..)
  , UserId(..)
  , UserIdentity(..)
  , ConversationState(..)
  , initialize
  , updateUser
  )
  where

import           Conversation (Conversation)
import           Conversation.Parameters.Split (Split)
import qualified Conversation.Parameters.Split as Split
import qualified Network.HTTP.Client as Http
import qualified Settings
import           Settings (Settings)
import qualified Splitwise
import qualified Telegram
import           Telegram.Api (ChatId)
import qualified Telegram.Username
import           Telegram.Username (Username)

data Model = Model
  { http :: Http.Manager
  , userA :: User
  , userB :: User
  , telegramToken :: Telegram.Token
  , splitwiseAuth :: Splitwise.Group
  }

data User = User
  { identity :: UserIdentity
  , preset :: Split
  , conversationState :: Maybe ConversationState
  }

data ConversationState
  = -- We have contacted the user before, and there is no active conversation
    Inactive ChatId
  | -- We are waiting for a reply from the user
    Active ChatId Conversation


data UserIdentity = UserIdentity
  { telegramId :: Username
  , splitwiseRole :: Splitwise.Role
  }

data UserId
  = UserA
  | UserB
  deriving (Show)

initialize :: Settings -> Http.Manager -> Model
initialize settings httpManager
  = let
      presetA = Settings.userASplitwisePreset settings
      presetB = 100 - presetA
    in
      Model
        { http          = httpManager
        , userA         = User
          { identity          = UserIdentity
            { telegramId    = ( Telegram.Username.fromString
                              . Settings.userATelegramId
                              )
              settings
            , splitwiseRole = Splitwise.Owner
            }
          , preset            = Split.init presetA
          , conversationState = Nothing
          }
        , userB         = User
          { identity          = UserIdentity
            { telegramId    = ( Telegram.Username.fromString
                              . Settings.userBTelegramId
                              )
              settings
            , splitwiseRole = Splitwise.Peer
            }
          , preset            = Split.init presetB
          , conversationState = Nothing
          }
        , telegramToken = Telegram.Token $ Settings.telegramToken settings
        , splitwiseAuth = Splitwise.group
          (Settings.userASplitwiseToken settings)
          (Settings.userASplitwiseId settings)
          (Settings.userBSplitwiseId settings)
        }

updateUser :: UserId -> User -> Model -> Model
updateUser userId updatedUser model = case userId of
  UserA -> model { userA = updatedUser }
  UserB -> model { userB = updatedUser }
