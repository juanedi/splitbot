module Splitwise
  ( createExpense
  , getBalance
  , UserId(..)
  , Token(..)
  , Balance
  )
where

import qualified Conversation.Expense
import qualified Conversation.Parameters.Amount as Amount
import qualified Conversation.Parameters.Description as Description
import           Conversation.Parameters.Split (Split)
import qualified Conversation.Parameters.Split as Split
import           Conversation.Parameters.Who
import qualified Currency
import           Data.ByteString.Char8 (pack)
import qualified Network.HTTP.Client as Http
import qualified Splitwise.Api as Api
import           Splitwise.Api.Balance (Balance)
import qualified Splitwise.Api.GetBalanceResponse as GetBalanceResponse

newtype UserId = UserId Integer

newtype Token = Token String

createExpense
  :: Http.Manager
  -> UserId
  -> UserId
  -> Token
  -> Conversation.Expense.Expense
  -> IO Bool
createExpense http (UserId currentUser) (UserId buddy) (Token token) expense =
  let description =
        (Description.text . Conversation.Expense.description) expense
      cost = (Amount.value . Conversation.Expense.amount) expense
      payer                          = Conversation.Expense.payer expense
      (myPaidShare, buddysPaidShare) = paidShares payer cost
      split                          = Conversation.Expense.split expense
      (myOwedShare, buddysOwedShare) = owedShares split cost
      apiToken                       = Api.Token $ pack token
  in  Api.createExpense http apiToken $ Api.Expense
        { Api.payment     = False
        , Api.cost        = cost
        , Api.currency    = Currency.ARS
        , Api.description = description
        , Api.user1Share  = Api.UserShare
          { Api.userId    = currentUser
          , Api.paidShare = myPaidShare
          , Api.owedShare = myOwedShare
          }
        , Api.user2Share  = Api.UserShare
          { Api.userId    = buddy
          , Api.paidShare = buddysPaidShare
          , Api.owedShare = buddysOwedShare
          }
        }

getBalance :: Http.Manager -> Token -> UserId -> IO (Maybe Balance)
getBalance http (Token token) (UserId friendId) = do
  response <- Api.getBalance http (Api.Token (pack token)) friendId
  return $ (GetBalanceResponse.balance . GetBalanceResponse.friend) <$> response

paidShares :: Who -> Integer -> (Integer, Integer)
paidShares payer cost = case payer of
  Me   -> (cost, 0)
  They -> (0, cost)

owedShares :: Split -> Integer -> (Integer, Integer)
owedShares split cost =
  let myShare =
        round $ (fromInteger (cost * (Split.myPart split)) :: Double) / 100
  in  (myShare, cost - myShare)
