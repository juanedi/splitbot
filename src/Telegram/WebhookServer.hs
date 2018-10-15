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
import           Telegram
import           Web.Scotty.Trans

newtype Config =
  Config
    { queue :: Queue Telegram.Message
    }

newtype ConfigM a =
  ConfigM
    { runConfigM :: ReaderT Config IO a
    }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadReader Config)

initState :: Queue Telegram.Message -> Config
initState q = Config {queue = q}

runIO :: Config -> ConfigM a -> IO a
runIO config m = runReaderT (runConfigM m) config

run :: Queue Telegram.Message -> Int -> IO ()
run q port = do
  let config = initState q
  scottyT port (runIO config) app

app :: ScottyT Text ConfigM ()
app = do
  middleware logStdoutDev
  post "/" $ do
    config <- lift ask
    rd     <- bodyReader
    body   <- liftIO rd
    case Aeson.eitherDecodeStrict body of
      Left _ -> liftIO $ putStrLn "Decoding error"
      Right update ->
        liftIO $ Queue.enqueue (queue config) (Telegram.toMessage update)
