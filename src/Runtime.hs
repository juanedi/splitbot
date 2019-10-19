module Runtime where

import qualified Core
import qualified Queue
import           Queue (Queue)
import           Settings (Settings)
import           Telegram.Message (Message)

start :: Settings -> Queue Message -> IO ()
start settings queue = loop queue (Core.initialize settings)

loop :: Queue Message -> Core.Model -> IO ()
loop queue model = do
  msg <- Queue.dequeue queue
  let event                    = Core.MessageReceived msg
      (updatedModel, _effects) = Core.update event model
  loop queue updatedModel
