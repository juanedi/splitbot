{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Telegram.WebhookServer (run) where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.Aeson as Aeson
import           Data.Text.Lazy (Text)
import           Network.Wai.Middleware.RequestLogger
import           Prelude
import           Telegram.Api (Token)
import           Telegram.Message (Message)
import qualified Telegram.Message as Message
import           Web.Scotty.Trans

newtype Config =
  Config
    { callback :: MessageCallback
    }

type MessageCallback = Message -> IO ()

newtype ConfigM a =
  ConfigM
    { runConfigM :: ReaderT Config IO a
    }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadReader Config)

run :: MessageCallback -> Token -> Int -> IO ()
run callback token port = do
  let config = initState callback
  scottyT port (runIO config) (app token)

initState :: MessageCallback -> Config
initState callback = Config {callback = callback}

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
            liftIO $ (callback config) (Message.fromUpdate update)
      else do
        liftIO $ putStrLn "Invalid token"
