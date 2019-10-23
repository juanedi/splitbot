module Main (main) where

import           Control.Concurrent.Async (concurrently)
import qualified Runtime
import qualified Settings
import qualified Telegram
import qualified Telegram.LongPolling

main :: IO ()
main = do
  settings <- Settings.fromEnv
  runtime  <- Runtime.initialize settings
  let token = Telegram.Token $ Settings.telegramToken settings
  _ <- concurrently
    (Runtime.start runtime)
    (Telegram.LongPolling.run (Runtime.onMessage runtime) token)
  return ()
