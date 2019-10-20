module Main where

import           Control.Concurrent.Async (concurrently)
import qualified Queue
import qualified Runtime
import qualified Settings
import qualified Telegram.WebhookServer

main :: IO ()
main = do
  settings <- Settings.fromEnv
  queue    <- Queue.new
  _        <- concurrently
    (Runtime.start settings queue)
    (Telegram.WebhookServer.run (\msg -> Queue.enqueue queue msg)
                                (Settings.telegramToken settings)
                                (Settings.port settings)
    )
  return ()
