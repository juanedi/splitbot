module Core where

import           Conversation.Parameters.Split (Split)
import qualified Conversation.Parameters.Split as Split
import           Core.Conversation (Conversation)
import qualified Settings
import           Settings (Settings)
import qualified Splitwise
import qualified Telegram
import           Telegram.Api (ChatId)
import           Telegram.Message
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

data Event
  = MessageReceived Telegram.Message.Message

type Effect = ()

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
  MessageReceived _msg -> (model, [])
