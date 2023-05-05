module Splitwise (
  Splitwise.init,
  createExpense,
  getBalance,
  Handler,
  Role (..),
  ExpenseOutcome (..),
) where

import Conversation.Expense (Split, Who (..))
import qualified Conversation.Expense as Expense
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


data Handler = Handler
  { http :: Http.Manager
  , token :: Api.Token
  , owner :: UserId
  , peer :: UserId
  }


data Role = Owner | Peer


data ExpenseOutcome = Created | Failed
  deriving (Show)


init :: Http.Manager -> String -> Integer -> Integer -> Handler
init http token owner peer =
  Handler
    { http = http
    , token = Api.Token (pack token)
    , owner = UserId owner
    , peer = UserId peer
    }


createExpense ::
  Handler ->
  Role ->
  Expense.Expense ->
  IO ExpenseOutcome
createExpense handler role expense = do
  let description =
        (Expense.descriptionText . Expense.description) expense
      cost = (Expense.amountValue . Expense.amount) expense
      payer = Expense.payer expense
      (ownerPaidShare, peerPaidShare) = paidShares payer role cost
      split = Expense.split expense
      (myOwedShare, buddysOwedShare) = owedShares split role cost
      apiToken = token handler
  success <-
    Api.createExpense (http handler) apiToken $
      Api.Expense
        { Api.payment = False
        , Api.cost = cost
        , Api.currency = Currency.ARS
        , Api.description = description
        , Api.user1Share =
            Api.UserShare
              { Api.userId = getId (owner handler)
              , Api.paidShare = ownerPaidShare
              , Api.owedShare = myOwedShare
              }
        , Api.user2Share =
            Api.UserShare
              { Api.userId = getId (peer handler)
              , Api.paidShare = peerPaidShare
              , Api.owedShare = buddysOwedShare
              }
        }
  return (if success then Created else Failed)


getBalance :: Handler -> Role -> IO (Maybe Balance)
getBalance handler role = do
  -- TODO use role to invert balance if necessary
  response <- Api.getBalance (http handler) (token handler) (getId (peer handler))
  let balance =
        GetBalanceResponse.balance . GetBalanceResponse.friend <$> response
  return
    ( case role of
        Owner -> balance
        Peer -> Balance.invert <$> balance
    )


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
        round $ (fromInteger (cost * Expense.myPart split) :: Double) / 100
   in case role of
        Owner -> (myShare, cost - myShare)
        Peer -> (cost - myShare, myShare)
