module Runtime where

import qualified Core
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Client.TLS (newTlsManager)
import qualified Queue
import           Queue (Queue)
import qualified Settings
import           Settings (Settings)
import qualified Splitwise
import qualified Telegram
import           Telegram.Message (Message)

data State = State
  { telegramToken :: Telegram.Token
  , splitwiseGroup :: Splitwise.Group
  , http :: Http.Manager
  , queue :: Queue Message
  }

start :: Settings -> Queue Message -> IO ()
start settings queue = do
  httpManager <- newTlsManager
  let state = State
        { telegramToken  = Telegram.Token $ Settings.telegramToken settings
        , splitwiseGroup = Splitwise.group
          (Settings.userASplitwiseToken settings)
          (Settings.userASplitwiseId settings)
          (Settings.userBSplitwiseId settings)
        , http           = httpManager
        , queue          = queue
        }
  loop state (Core.initialize settings)

loop :: State -> Core.Model -> IO ()
loop state model = do
  msg <- Queue.dequeue (queue state)
  let event                   = (Core.MessageReceived msg)
      (updatedModel, effects) = Core.update event model
  runEffects effects
  loop state updatedModel

runEffects :: [Core.Effect] -> IO ()
runEffects effects = case effects of
  [] -> return ()
  first : rest ->
    -- TODO: catch errors
    (runEffect first) >> runEffects rest

runEffect :: Core.Effect -> IO ()
runEffect effect = case effect of
  Core.LogError msg -> putStrLn msg
