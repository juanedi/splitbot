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
  }

fromEnv :: IO (Settings)
fromEnv =
  Settings
    <$> getEnv "USER_A_TG_ID"
    <*> getEnvInt "USER_A_SW_ID"
    <*> getEnvInt "USER_A_PRESET"
    <*> getEnv "USER_B_TG_ID"
    <*> getEnvInt "USER_B_SW_ID"
    <*> getEnv "TELEGRAM_TOKEN"
    <*> getEnv "SPLITWISE_TOKEN"

getEnvInt :: String -> IO Integer
getEnvInt key = read <$> getEnv key
