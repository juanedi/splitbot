module Splitwise (
  group,
  createExpense,
  getBalance,
  Group,
  Role (..),
  ExpenseOutcome (..),
) where

import qualified Conversation.Expense
import qualified Conversation.Parameters.Amount as Amount
import qualified Conversation.Parameters.Description as Description
import Conversation.Parameters.Split (Split)
import qualified Conversation.Parameters.Split as Split
import Conversation.Parameters.Who
import qualified Currency
import Data.ByteString.Char8 (pack)
import qualified Network.HTTP.Client as Http
import qualified Splitwise.Api as Api
import Splitwise.Api.Balance (Balance)
import qualified Splitwise.Api.Balance as Balance
import qualified Splitwise.Api.GetBalanceResponse as GetBalanceResponse


newtype UserId = UserId
  { getId :: Integer
  }
  deriving (Eq)


data Group = Group
  { token :: Api.Token
  , owner :: UserId
  , peer :: UserId
  }


data Role = Owner | Peer


data ExpenseOutcome = Created | Failed
  deriving (Show)


group :: String -> Integer -> Integer -> Group
group token owner peer =
  Group
    { token = Api.Token (pack token)
    , owner = UserId owner
    , peer = UserId peer
    }


createExpense ::
  Http.Manager ->
  Role ->
  Group ->
  Conversation.Expense.Expense ->
  IO ExpenseOutcome
createExpense http role group expense = do
  let description =
        (Description.text . Conversation.Expense.description) expense
      cost = (Amount.value . Conversation.Expense.amount) expense
      payer = Conversation.Expense.payer expense
      (ownerPaidShare, peerPaidShare) = paidShares payer role cost
      split = Conversation.Expense.split expense
      (myOwedShare, buddysOwedShare) = owedShares split role cost
      apiToken = token group
  success <-
    Api.createExpense http apiToken $
      Api.Expense
        { Api.payment = False
        , Api.cost = cost
        , Api.currency = Currency.ARS
        , Api.description = description
        , Api.user1Share =
            Api.UserShare
              { Api.userId = getId (owner group)
              , Api.paidShare = ownerPaidShare
              , Api.owedShare = myOwedShare
              }
        , Api.user2Share =
            Api.UserShare
              { Api.userId = getId (peer group)
              , Api.paidShare = peerPaidShare
              , Api.owedShare = buddysOwedShare
              }
        }
  return (if success then Created else Failed)


getBalance :: Http.Manager -> Group -> Role -> IO (Maybe Balance)
getBalance http group role = do
  -- TODO use role to invert balance if necessary
  response <- Api.getBalance http (token group) (getId (peer group))
  let balance =
        (GetBalanceResponse.balance . GetBalanceResponse.friend) <$> response
  return $ case role of
    Owner -> balance
    Peer -> Balance.invert <$> balance


-- (owner_share, peer_share)
paidShares :: Who -> Role -> Integer -> (Integer, Integer)
paidShares Me Owner cost = (cost, 0)
paidShares Me Peer cost = (0, cost)
paidShares They Owner cost = (0, cost)
paidShares They Peer cost = (cost, 0)


-- (owner_share, peer_share)
owedShares :: Split -> Role -> Integer -> (Integer, Integer)
owedShares split role cost =
  let myShare =
        round $ (fromInteger (cost * (Split.myPart split)) :: Double) / 100
   in case role of
        Owner -> (myShare, cost - myShare)
        Peer -> (cost - myShare, myShare)
