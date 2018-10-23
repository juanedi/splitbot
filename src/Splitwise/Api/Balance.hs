module Splitwise.Api.Balance
  ( Balance
  , currency
  , amount ) where

import Currency (Currency)
import Data.Aeson ( FromJSON , (.:) , parseJSON , withObject)

type Balance = [ CurrencyBalance ]

data CurrencyBalance =
  CurrencyBalance
    { currency :: Currency
    , amount :: Float
    }
    deriving Show

instance FromJSON CurrencyBalance where
  parseJSON =
    withObject "balance" $ \o ->
      CurrencyBalance
        <$> (o .: "currency_code")
        <*> -- the amount comes as a string
            (fmap read (o .: "amount"))
