module Main (main) where

import Runtime
import System.Environment
import System.Exit

main :: IO ()
main = do
  lcAll <- System.Environment.lookupEnv "LC_ALL"
  args <- System.Environment.getArgs
  case lcAll of
    Nothing -> pure ()
    Just val -> putStrLn ("------- LC_ALL: " ++ val)
  case args of
    ["--polling"] -> do
      putStrLn "========================================="
      putStrLn "Starting bot in polling mode 🚀"
      Runtime.startPolling
    ["--server" ] -> do
      putStrLn "Starting bot with webserver 🚀"
      Runtime.startServer
    _             -> System.Exit.die
      "Usage: splitbot MODE, where MODE is wither --polling or --server"
