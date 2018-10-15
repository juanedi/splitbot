module Main where

import           Control.Concurrent.Async (concurrently)
import qualified Queue
import qualified Server
import qualified Settings
import qualified Worker

main :: IO ()
main = do
  settings <- Settings.fromEnv
  queue    <- Queue.new
  _        <- concurrently (Worker.run settings queue) (Server.run queue 3000)
  return ()
