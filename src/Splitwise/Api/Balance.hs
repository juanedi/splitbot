module Splitwise.Api.Balance (
  Balance,
  CurrencyBalance,
  currency,
  amount,
  invert,
) where

import Currency (Currency)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))


type Balance = [CurrencyBalance]


data CurrencyBalance = CurrencyBalance
  { currency :: Currency
  , amount :: Float
  }
  deriving (Show)


instance FromJSON CurrencyBalance where
  parseJSON =
    withObject "balance" $ \o ->
      CurrencyBalance
        <$> (o .: "currency_code")
        <*> (fmap read (o .: "amount")) -- the amount comes as a string


invert :: Balance -> Balance
invert = fmap (\(CurrencyBalance c a) -> CurrencyBalance c (-a))
