{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Telegram.WebhookServer (run) where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.Aeson as Aeson
import           Data.Text.Lazy (Text)
import           Network.Wai.Middleware.RequestLogger
import           Prelude
import qualified Queue
import           Queue (Queue)
import           Telegram.Api (Token)
import           Telegram.Message (Message)
import qualified Telegram.Message as Message
import           Web.Scotty.Trans

newtype Config =
  Config
    { queue :: Queue Message
    }

newtype ConfigM a =
  ConfigM
    { runConfigM :: ReaderT Config IO a
    }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadReader Config)

run :: Queue Message -> Token -> Int -> IO ()
run q token port = do
  let config = initState q
  scottyT port (runIO config) (app token)

initState :: Queue Message -> Config
initState q = Config {queue = q}

runIO :: Config -> ConfigM a -> IO a
runIO config m = runReaderT (runConfigM m) config

app :: Token -> ScottyT Text ConfigM ()
app telegramToken = do
  middleware logStdoutDev
  post "/:token/updates" $ do
    token <- param "token"
    if token == telegramToken
      then do
        config <- lift ask
        rd     <- bodyReader
        body   <- liftIO rd
        case Aeson.eitherDecodeStrict body of
          Left _ -> liftIO $ putStrLn "Decoding error"
          Right update ->
            liftIO $ Queue.enqueue (queue config) (Message.fromUpdate update)
      else do
        liftIO $ putStrLn "Invalid token"
