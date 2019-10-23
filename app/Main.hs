module Main (main) where

import Runtime
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    ["--polling"] -> Runtime.startPolling
    ["--server" ] -> Runtime.startServer
    _             -> System.Exit.die
      "Usage: splitbot MODE, where MODE is wither --polling or --server"
