module Main (main) where

import Runtime
import qualified Settings

main :: IO ()
main = do
  settings <- Settings.fromEnv
  Runtime.start settings
