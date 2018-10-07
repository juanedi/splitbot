module Main where

import Control.Arrow ((>>>))
import Conversation
import Conversation.Parameters (Split(..))
import Data.Traversable (sequence)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Settings
import Telegram.Api (ChatId, Reply(..))
import Telegram (Message)
import qualified Telegram
import qualified Splitwise

data State = State
  { http :: Http.Manager
  , userA :: User
  , userB :: User
  , telegram :: Telegram.State
  , splitwise :: Splitwise.State
  }

data User = User
  { identity :: UserIdentity
  , preset :: Split
  , conversation :: Maybe Conversation
  }

data UserIdentity = UserIdentity
  { telegramId :: Telegram.Username
  , splitwiseId :: Splitwise.UserId
  }

data ChatState = ChatState
  { currentUser :: UserIdentity
  , buddy :: UserIdentity
  , chatId :: ChatId
  }

data UserId
  = UserA
  | UserB
  deriving (Show)

main :: IO ()
main = do
  settings    <- Settings.fromEnv
  httpManager <- newTlsManager
  let presetA = Settings.userAPreset settings
      presetB = 100 - presetA
  runServer $ State
    { http      = httpManager
    , userA     = User
      { identity     = UserIdentity
        { telegramId  = (Telegram.Username . Settings.userATelegramId) settings
        , splitwiseId = (Splitwise.UserId . Settings.userASplitwiseId) settings
        }
      , preset       = Split presetA
      , conversation = Nothing
      }
    , userB     = User
      { identity     = UserIdentity
        { telegramId  = (Telegram.Username . Settings.userBTelegramId) settings
        , splitwiseId = (Splitwise.UserId . Settings.userBSplitwiseId) settings
        }
      , preset       = Split presetB
      , conversation = Nothing
      }
    , telegram  = Telegram.init (Settings.telegramToken settings)
    , splitwise = Splitwise.init
      $ (Splitwise.Token . Settings.splitwiseToken) settings
    }

runServer :: State -> IO ()
runServer state = do
  (message, newState) <- getMessage state
  let username    = Telegram.username message
  let maybeUserId = matchUserId state username
  case maybeUserId of
    Nothing -> do
      putStrLn $ "Ignoring message from unknown user" ++ show username
      runServer newState
    Just userId -> processMessage newState userId message >>= runServer

getMessage :: State -> IO (Message, State)
getMessage state = do
  let manager = http state
  (message, newTelegramState) <- (telegram >>> Telegram.getMessage manager)
    state
  putStrLn $ "<< " ++ (Telegram.text message)
  return $ (message, state { telegram = newTelegramState })

matchUserId :: State -> Telegram.Username -> Maybe UserId
matchUserId state uname
  | (userA >>> identity >>> telegramId) state == uname = Just UserA
  | (userB >>> identity >>> telegramId) state == uname = Just UserB
  | otherwise = Nothing

processMessage :: State -> UserId -> Message -> IO (State)
processMessage state userId message
  = let
      currentUser                    = getUser userId state
      otherUser                      = getUser (otherUserId userId) state
      currentConversation            = conversation currentUser
      (updatedConversation, effects) = case currentConversation of
        Nothing -> start (preset currentUser)
        Just c  -> advance (Telegram.text message) c
      updatedState = updateUser
        userId
        (currentUser { conversation = updatedConversation })
        state
      chatState = ChatState
        { currentUser = identity currentUser
        , buddy       = identity otherUser
        , chatId      = Telegram.chatId message
        }
    in
      do
        _ <- runEffects state chatState effects
        return updatedState

runEffects :: State -> ChatState -> [Effect] -> IO ()
runEffects state chatState effects = do
  _ <- sequence (run <$> effects)
  return ()
  where run = runEffect state chatState

runEffect :: State -> ChatState -> Effect -> IO ()
runEffect state chatState effect =
  let httpManager    = http state
      telegramState  = telegram state
      splitwiseState = splitwise state
  in  case effect of
        Answer reply ->
          sendMessage httpManager telegramState (chatId chatState) reply
        StoreAndReply expense reply -> do
          sucess <- Splitwise.createExpense
            httpManager
            (splitwiseId (currentUser chatState))
            (splitwiseId (buddy chatState))
            expense
            splitwiseState
          if sucess
            then sendMessage httpManager telegramState (chatId chatState) reply
            else return ()

sendMessage :: Http.Manager -> Telegram.State -> ChatId -> Reply -> IO ()
sendMessage http telegram chatId reply@(Reply text _) = do
  putStrLn $ ">> " ++ text
  Telegram.sendMessage http telegram chatId reply

getUser :: UserId -> State -> User
getUser userId = case userId of
  UserA -> userA
  UserB -> userB

otherUserId :: UserId -> UserId
otherUserId userId = case userId of
  UserA -> UserB
  UserB -> UserA

updateUser :: UserId -> User -> State -> State
updateUser userId updatedUser state = case userId of
  UserA -> state { userA = updatedUser }
  UserB -> state { userB = updatedUser }
