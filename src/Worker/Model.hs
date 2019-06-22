module Worker.Model
  ( Model(..)
  , User(..)
  , UserId(..)
  , UserIdentity(..)
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
  , conversation :: Maybe Conversation
  , chatId :: Maybe ChatId
  }

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
          { identity     = UserIdentity
            { telegramId    = ( Telegram.Username.fromString
                              . Settings.userATelegramId
                              )
              settings
            , splitwiseRole = Splitwise.Owner
            }
          , preset       = Split.init presetA
          , conversation = Nothing
          , chatId       = Nothing
          }
        , userB         = User
          { identity     = UserIdentity
            { telegramId    = ( Telegram.Username.fromString
                              . Settings.userBTelegramId
                              )
              settings
            , splitwiseRole = Splitwise.Peer
            }
          , preset       = Split.init presetB
          , conversation = Nothing
          , chatId       = Nothing
          }
        , telegramToken = Telegram.Token $ Settings.telegramToken settings
        , splitwiseAuth = Splitwise.group
          (Settings.userASplitwiseToken settings)
          (Settings.userASplitwiseId settings)
          (Settings.userBSplitwiseId settings)
        }

updateUser :: UserId -> Model -> User -> Model
updateUser userId model updatedUser = case userId of
  UserA -> model { userA = updatedUser }
  UserB -> model { userB = updatedUser }
