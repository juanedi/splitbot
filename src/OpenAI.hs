module OpenAI (OpenAI.init, Handler) where

import qualified Network.HTTP.Client as Http


data Handler = Handler
  { http :: Http.Manager
  , token :: String
  }


init :: Http.Manager -> String -> Handler
init http_ token_ =
  Handler {http = http_, token = token_}
