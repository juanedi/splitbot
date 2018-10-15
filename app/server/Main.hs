module Main where

import           Control.Concurrent.Async (concurrently)
import qualified Queue
import qualified Settings
import qualified Telegram.WebhookServer
import qualified Worker

main :: IO ()
main = do
  settings <- Settings.fromEnv
  queue    <- Queue.new
  _        <- concurrently (Worker.run settings queue)
                           (Telegram.WebhookServer.run queue 3000)
  return ()
