module Settings
  ( Settings(..)
  , fromEnv
  ) where

import System.Environment (getEnv)

data Settings = Settings
  { userA :: String
  , userB :: String
  , databaseUrl :: String
  , telegramToken :: String
  }

fromEnv :: IO (Settings)
fromEnv =
  Settings <$> getEnv "USER_A" <*> getEnv "USER_B" <*> getEnv "DB_URL" <*>
  getEnv "TELEGRAM_TOKEN"
