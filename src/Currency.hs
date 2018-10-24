{-# LANGUAGE OverloadedStrings #-}
module Currency
  ( Currency(..)
  , code
  ) where

import           Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, withText)
import qualified Data.Text as Text

data Currency = ARS | USD
  deriving (Eq, Ord)

instance Show Currency where
  show c = code c

code :: Currency -> String
code currency = case currency of
  ARS -> "ARS"
  USD -> "USD"


instance ToJSON Currency where
  toJSON c = toJSON (code c)


instance FromJSON Currency where
  parseJSON =
    withText "currency" (\c ->
                           case c of
                             "ARS" -> return ARS
                             "USD" -> return USD
                             _ -> fail ("Unrecognized currency: " ++ (Text.unpack c))
                           )
