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
  let event                   = (Core.MessageReceived msg)
      (updatedModel, effects) = Core.update event model
  runEffects effects
  loop queue updatedModel

runEffects :: [Core.Effect] -> IO ()
runEffects effects = case effects of
  [] -> return ()
  first : rest ->
    -- TODO: catch errors
    (runEffect first) >> runEffects rest

runEffect :: Core.Effect -> IO ()
runEffect effect = case effect of
  Core.LogError msg -> putStrLn msg
