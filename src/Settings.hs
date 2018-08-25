module Settings
  ( Settings(..)
  , fromEnv
  ) where

import System.Environment (getEnv)

data Settings = Settings
  { userA :: String
  , userB :: String
  , presetA :: Integer
  , databaseUrl :: String
  , telegramToken :: String
  }

fromEnv :: IO (Settings)
fromEnv =
  Settings <$> getEnv "USER_A" <*> getEnv "USER_B" <*> getEnvInt "PRESET_A" <*>
  getEnv "DB_URL" <*>
  getEnv "TELEGRAM_TOKEN"

getEnvInt :: String -> IO Integer
getEnvInt key = read <$> getEnv key
