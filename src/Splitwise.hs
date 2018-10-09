module Splitwise
  ( Splitwise.init
  , createExpense
  , State
  , UserId(..)
  , Token(..)
  )
where

import qualified Conversation.Expense
import qualified Network.HTTP.Client as Http
import qualified Splitwise.Api as Api
import qualified Conversation.Parameters as Parameters
import Data.ByteString.Char8 (pack)

data State = State Token

data UserId = UserId Integer

data Token = Token String


init :: Token -> State
init = State


createExpense
  :: Http.Manager
  -> UserId
  -> UserId
  -> Conversation.Expense.Expense
  -> State
  -> IO Bool
createExpense http (UserId currentUser) (UserId buddy) expense (State (Token token))
  = let description =
          Parameters.text $ Conversation.Expense.description expense
        cost = Parameters.value $ Conversation.Expense.amount expense
        payer                          = Conversation.Expense.payer expense
        (myPaidShare, buddysPaidShare) = paidShares payer cost
        split                          = Conversation.Expense.split expense
        (myOwedShare, buddysOwedShare) = owedShares split cost
        apiToken                       = Api.Token $ pack token
    in  Api.createExpense http apiToken $ Api.Expense
          { Api.payment     = False
          , Api.cost        = cost
          , Api.currency    = Api.ARS
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

paidShares :: Parameters.Who -> Integer -> (Integer, Integer)
paidShares payer cost = case payer of
  Parameters.Me   -> (cost, 0)
  Parameters.They -> (0, cost)

owedShares :: Parameters.Split -> Integer -> (Integer, Integer)
owedShares split cost =
  let myShare =
        round $ (fromInteger (cost * (Parameters.myPart split)) :: Double) / 100
  in  (myShare, cost - myShare)
