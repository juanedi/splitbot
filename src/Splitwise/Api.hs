module Splitwise.Api (
  Currency(..),
  Expense(..),
  UserShare(..),
  Token(..),
  createExpense
  ) where

import           Data.Aeson (ToJSON, (.=), object, toJSON)
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Network.HTTP.Client (Request, RequestBody(..))
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Status

data Token = Token ByteString

data Expense = Expense
  { payment :: Bool
  , cost :: Amount
  , currency :: Currency
  , description :: String
  , user1Share :: UserShare
  , user2Share :: UserShare
  }

type Amount = Integer

data Currency
  = ARS
  | USD

data UserShare = UserShare
  { userId :: Integer
  , paidShare :: Amount
  , owedShare :: Amount
  }

instance ToJSON Expense where
  toJSON e =
    object $
    [ "payment" .= False
    , "cost" .= cost e
    , "description" .= description e
    , "currency_code" .= (currencyCode . currency) e
    , "users__0__user_id" .= (userId . user1Share) e
    , "users__0__paid_share" .= (paidShare . user1Share) e
    , "users__0__owed_share" .= (owedShare . user1Share) e
    , "users__1__user_id" .= (userId . user2Share) e
    , "users__1__paid_share" .= (paidShare . user2Share) e
    , "users__1__owed_share" .= (owedShare . user2Share) e
    ]

currencyCode :: Currency -> String
currencyCode currency = case currency of
  ARS -> "ARS"
  USD -> "USD"

createExpense :: Http.Manager -> Token -> Expense -> IO Bool
createExpense manager token expense = do
  request  <- newExpenseRequest token expense
  response <- Http.httpLbs request manager
  let statusCode = Status.statusCode (Http.responseStatus response)
  return (statusCode == 200)

newExpenseRequest :: Token -> Expense -> IO Request
newExpenseRequest (Token token) expense =
  return
    $ (Http.parseRequest_ " https://www.splitwise.com/api/v3.0/create_expense")
        { Http.method         = "POST"
        , Http.requestHeaders = [ ("Content-Type", "application/json")
                                , ( "Authorization"
                                  , BS.concat ["Bearer ", token]
                                  )
                                ]
        , Http.requestBody    = (RequestBodyLBS . Aeson.encode) expense
        }
