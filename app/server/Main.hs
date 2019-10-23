module Main where

import           Control.Concurrent.Async (concurrently)
import qualified Runtime
import qualified Settings
import qualified Telegram.WebhookServer

main :: IO ()
main = do
  settings <- Settings.fromEnv
  runtime  <- Runtime.initialize settings
  _        <- concurrently
    (Runtime.start runtime)
    (Telegram.WebhookServer.run (Runtime.onMessage runtime)
                                (Settings.telegramToken settings)
                                (Settings.port settings)
    )
  return ()
