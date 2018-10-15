module Main where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (concurrently)
import qualified Queue
import Queue (Queue)
import qualified Server

main :: IO ()
main = do
  q <- Queue.new
  _ <- concurrently (reader q) (Server.run q 3000)
  return ()

reader :: Queue String -> IO ()
reader q = do
  msg <- Queue.dequeue q
  putStrLn msg
  reader q

sleep :: Int -> IO ()
sleep seconds = threadDelay (seconds * 1000 * 1000)
