module Splitwise.Api (
  Expense (..),
  UserShare (..),
  Token (..),
  createExpense,
  getBalance,
) where

import Control.Exception as Exception
import Currency (Currency)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client (Request, RequestBody (..))
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Status
import Splitwise.Api.GetBalanceResponse (GetBalanceResponse)


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
      , "currency_code" .= currency e
      , "users__0__user_id" .= (userId . user1Share) e
      , "users__0__paid_share" .= (paidShare . user1Share) e
      , "users__0__owed_share" .= (owedShare . user1Share) e
      , "users__1__user_id" .= (userId . user2Share) e
      , "users__1__paid_share" .= (paidShare . user2Share) e
      , "users__1__owed_share" .= (owedShare . user2Share) e
      ]


createExpense :: Http.Manager -> Token -> Expense -> IO Bool
createExpense manager token expense = do
  request <- newExpenseRequest token expense
  response <- Http.httpLbs request manager
  let statusCode = Status.statusCode (Http.responseStatus response)
  return (statusCode == 200)


newExpenseRequest :: Token -> Expense -> IO Request
newExpenseRequest (Token token) expense =
  return $
    (Http.parseRequest_ " https://www.splitwise.com/api/v3.0/create_expense")
      { Http.method = "POST"
      , Http.requestHeaders =
          [ ("Content-Type", "application/json")
          ,
            ( "Authorization"
            , BS.concat ["Bearer ", token]
            )
          ]
      , Http.requestBody = (RequestBodyLBS . Aeson.encode) expense
      }


getBalance :: Http.Manager -> Token -> Integer -> IO (Maybe GetBalanceResponse)
getBalance manager token friendId = do
  request <- newBalanceRequest token friendId
  result <- runRequest request manager
  case result of
    Left _err -> do
      putStrLn "runtime error"
      return Nothing
    Right response ->
      case (Status.statusCode . Http.responseStatus) response of
        200 -> case Aeson.eitherDecode (Http.responseBody response) of
          Left _ -> do
            putStrLn (show (Http.responseBody response))
            putStrLn "decoding failure"
            return Nothing
          Right balance -> do
            putStrLn "ok!"
            return (Just balance)
        code -> do
          putStrLn ("invalid status code: " ++ (show code))
          return Nothing


newBalanceRequest :: Token -> Integer -> IO Request
newBalanceRequest (Token token) friendId =
  return $
    ( Http.parseRequest_
        ("https://www.splitwise.com/api/v3.0/get_friend/" ++ (show friendId))
    )
      { Http.method = "GET"
      , Http.requestHeaders =
          [ ("Content-Type", "application/json")
          ,
            ( "Authorization"
            , BS.concat ["Bearer ", token]
            )
          ]
      }


runRequest ::
  Http.Request ->
  Http.Manager ->
  IO (Either Http.HttpException (Http.Response LBS.ByteString))
runRequest request manager = Exception.try (Http.httpLbs request manager)
