module Telegram.Username (Username, fromString) where


data Username
  = Username String


instance Eq Username where
  (==) (Username a) (Username b) = a == b


instance Show Username where
  show (Username a) = a


fromString :: String -> Username
fromString = Username
