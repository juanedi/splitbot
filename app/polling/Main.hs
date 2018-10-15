module Main (main) where

import           Control.Concurrent.Async (concurrently)
import qualified Queue
import qualified Settings
import qualified Telegram
import qualified Telegram.LongPolling
import qualified Worker

main :: IO ()
main = do
  settings <- Settings.fromEnv
  queue    <- Queue.new
  let token = Telegram.Token $ Settings.telegramToken settings
  _ <- concurrently (Worker.run settings queue)
                    (Telegram.LongPolling.run queue token)
  return ()
