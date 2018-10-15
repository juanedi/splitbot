module Settings
  ( Settings(..)
  , fromEnv
  ) where

import System.Environment (getEnv)

data Settings = Settings
  { userATelegramId :: String
  , userASplitwiseId :: Integer
  , userAPreset :: Integer
  , userBTelegramId :: String
  , userBSplitwiseId :: Integer
  , telegramToken :: String
  , splitwiseToken :: String
  , port :: Int
  }

fromEnv :: IO (Settings)
fromEnv =
  Settings
    <$> getEnv "USER_A_TG_ID"
    <*> readEnv "USER_A_SW_ID"
    <*> readEnv "USER_A_PRESET"
    <*> getEnv "USER_B_TG_ID"
    <*> readEnv "USER_B_SW_ID"
    <*> getEnv "TELEGRAM_TOKEN"
    <*> getEnv "SPLITWISE_TOKEN"
    <*> readEnv "PORT"

readEnv :: Read a => String -> IO a
readEnv key = read <$> getEnv key
