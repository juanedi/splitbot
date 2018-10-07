module Splitwise
  ( Splitwise.init
  , State
  , UserId(..)
  , Token(..)
  )
where


data State = State
  { token :: Token
  }

data UserId = UserId Integer

data Token = Token String


init :: Token -> State
init = State
