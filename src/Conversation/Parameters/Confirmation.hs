module Conversation.Parameters.Confirmation (
    ask,
    Conversation.Parameters.Confirmation.read,
) where

import Conversation.Expense (Expense)
import qualified Conversation.Expense as Expense
import qualified Conversation.Parameters.Amount as Amount
import qualified Conversation.Parameters.Description as Description
import qualified Conversation.Parameters.Split as Split
import Conversation.Parameters.Who
import Telegram.Reply (Reply)
import qualified Telegram.Reply as Reply


ask :: Expense -> Reply
ask expense =
    let descriptionLine =
            concat ["*", (Description.text . Expense.description) expense, "*\n"]

        payerLine =
            concat
                [ "Payed by "
                , case (Expense.payer expense) of
                    Me -> "me"
                    They -> "them"
                , "\n"
                ]

        amountLine =
            concat ["Total: $", show $ (Amount.value . Expense.amount) expense, "\n"]

        splitLine =
            concat ["I owe ", show $ Split.myPart (Expense.split expense), "%", "\n"]
     in Reply.withOptions
            ( concat
                [ "Is this correct❓\n\n"
                , descriptionLine
                , amountLine
                , payerLine
                , splitLine
                ]
            )
            ["Yes", "No"]


read :: String -> Bool
read "Yes" = True
read _ = False
