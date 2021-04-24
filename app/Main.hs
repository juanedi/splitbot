module Main (main) where

import Runtime
import qualified Settings
import System.Environment
import System.Exit

main :: IO ()
main = do
  settings <- Settings.fromEnv
  args <- System.Environment.getArgs
  case args of
    ["--polling"] -> do
      putStrLn "========================================="
      putStrLn "Starting bot in polling mode 🚀"
      Runtime.startPolling settings
    ["--server" ] -> do
      putStrLn "Starting bot with webserver 🚀"
      Runtime.startServer settings
    _             -> System.Exit.die
      "Usage: splitbot MODE, where MODE is wither --polling or --server"
