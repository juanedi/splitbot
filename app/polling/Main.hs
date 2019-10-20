module Main (main) where

import           Control.Concurrent.Async (concurrently)
import qualified Queue
import qualified Runtime
import qualified Settings
import qualified Telegram
import qualified Telegram.LongPolling

main :: IO ()
main = do
  settings <- Settings.fromEnv
  queue    <- Queue.new
  let token = Telegram.Token $ Settings.telegramToken settings
  _ <- concurrently (Runtime.start settings queue)
                    (Telegram.LongPolling.run (Runtime.onMessage queue) token)
  return ()
