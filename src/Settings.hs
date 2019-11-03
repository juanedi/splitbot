module Settings
  ( Settings(..)
  , fromEnv
  ) where

import System.Environment (getEnv)

data Settings = Settings
  { -- Telegram ID for user A. That's the username on your profile without the leading '@'
    userATelegramId :: String
  , -- Splitwise API token. The token must be generated by one of the users of the bot.
    userASplitwiseToken :: String
  , -- Splitwise user ID for user A. You can use the API to fetch this.
    userASplitwiseId :: Integer
  , -- Default owned share for expenses, expresed as a percentage. The share for
    -- the other user is computed by subtracting this value from 100%.
    userASplitwisePreset :: Integer
  , -- Telegram ID for user B.
    userBTelegramId :: String
  , -- Splitwise user ID for user B. You can use the API to fetch this.
    userBSplitwiseId :: Integer
  , -- Telegram API token
    telegramToken :: String
  , -- Port number for the server if using webhooks. This will be ignored if using long polling.
    port :: Int
  , -- Were to store persisted data
    storePath :: FilePath
  }

fromEnv :: IO (Settings)
fromEnv =
  Settings
    <$> getEnv "USER_A_TG_ID"
    <*> getEnv "USER_A_SW_TOKEN"
    <*> readEnv "USER_A_SW_ID"
    <*> readEnv "USER_A_SW_PRESET"
    <*> getEnv "USER_B_TG_ID"
    <*> readEnv "USER_B_SW_ID"
    <*> getEnv "TELEGRAM_TOKEN"
    <*> readEnv "PORT"
    <*> getEnv "STORE_PATH"

readEnv :: Read a => String -> IO a
readEnv key = read <$> getEnv key
