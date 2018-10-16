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
import qualified Telegram.Username
import           Telegram.Username (Username)

data Model = Model
  { http :: Http.Manager
  , userA :: User
  , userB :: User
  , telegramToken :: Telegram.Token
  , splitwiseToken :: Splitwise.Token
  }

data User = User
  { identity :: UserIdentity
  , preset :: Split
  , conversation :: Maybe Conversation
  }

data UserIdentity = UserIdentity
  { telegramId :: Username
  , splitwiseId :: Splitwise.UserId
  }

data UserId
  = UserA
  | UserB
  deriving (Show)

initialize :: Settings -> Http.Manager -> Model
initialize settings httpManager
  = let
      presetA = Settings.userAPreset settings
      presetB = 100 - presetA
    in
      Model
        { http           = httpManager
        , userA          = User
          { identity     = UserIdentity
            { telegramId  = ( Telegram.Username.fromString
                            . Settings.userATelegramId
                            )
              settings
            , splitwiseId = (Splitwise.UserId . Settings.userASplitwiseId)
              settings
            }
          , preset       = Split.init presetA
          , conversation = Nothing
          }
        , userB          = User
          { identity     = UserIdentity
            { telegramId  = ( Telegram.Username.fromString
                            . Settings.userBTelegramId
                            )
              settings
            , splitwiseId = (Splitwise.UserId . Settings.userBSplitwiseId)
              settings
            }
          , preset       = Split.init presetB
          , conversation = Nothing
          }
        , telegramToken  = Telegram.Token $ Settings.telegramToken settings
        , splitwiseToken = Splitwise.Token $ Settings.splitwiseToken settings
        }

updateUser :: UserId -> Model -> User -> Model
updateUser userId model updatedUser = case userId of
  UserA -> model { userA = updatedUser }
  UserB -> model { userB = updatedUser }
