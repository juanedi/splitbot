{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Server (run) where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.String
import           Data.Text.Lazy (Text)
import           Network.Wai.Middleware.RequestLogger
import           Prelude
import qualified Queue
import           Queue (Queue)
import           Web.Scotty.Trans

newtype Config =
  Config
    { queue :: Queue String
    }

newtype ConfigM a =
  ConfigM
    { runConfigM :: ReaderT Config IO a
    }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadReader Config)

initState :: Queue String -> Config
initState q = Config {queue = q}

runIO :: Config -> ConfigM a -> IO a
runIO config m = runReaderT (runConfigM m) config

run :: Queue String -> Int -> IO ()
run q port = do
  let config = initState q
  scottyT port (runIO config) app

app :: ScottyT Text ConfigM ()
app = do
  middleware logStdoutDev
  get "/:msg" $ do
    config <- lift ask
    msg    <- param "msg"
    liftIO $ Queue.enqueue (queue config) msg
