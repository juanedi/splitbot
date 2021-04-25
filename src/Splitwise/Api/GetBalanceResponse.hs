{-# LANGUAGE FlexibleInstances #-}

module Splitwise.Api.GetBalanceResponse (GetBalanceResponse, friend, balance) where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Splitwise.Api.Balance (Balance)


data GetBalanceResponse = GetBalanceResponse
    {friend :: Friend}


instance FromJSON GetBalanceResponse where
    parseJSON =
        withObject "response" $ \o ->
            GetBalanceResponse <$> (o .: "friend")


data Friend = Friend
    { balance :: Balance
    }


instance FromJSON Friend where
    parseJSON =
        withObject "balance" $ \o ->
            Friend <$> (o .: "balance")
