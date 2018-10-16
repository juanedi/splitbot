module Worker (run) where

import qualified Conversation
import           Conversation (Conversation, Effect(..))
import           Conversation.Parameters.Split (Split)
import qualified Conversation.Parameters.Split as Split
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Client.TLS (newTlsManager)
import qualified Queue
import           Queue (Queue)
import qualified Settings
import           Settings (Settings)
import qualified Splitwise
import qualified Telegram
import           Telegram.Api (ChatId)
import           Telegram.Message (Message)
import qualified Telegram.Message as Message
import qualified Telegram.Reply as Reply
import qualified Telegram.Username
import           Telegram.Username (Username)

data State = State
  { http :: Http.Manager
  , userA :: User
  , userB :: User
  , telegramToken :: Telegram.Token
  , splitwiseToken :: Splitwise.Token
  }

data User = User
  { identity :: UserIdentity
  , preset :: Split
  , conversation :: Maybe Conversation
  }

data UserIdentity = UserIdentity
  { telegramId :: Telegram.Username.Username
  , splitwiseId :: Splitwise.UserId
  }

data UserId
  = UserA
  | UserB
  deriving (Show)

data Session = Session
  { userId :: UserId
  , chatId :: ChatId
  , user :: User
  , buddy :: UserIdentity
  }

run :: Settings -> Queue Message -> IO ()
run settings queue = do
  httpManager <- newTlsManager
  loop queue (initState settings httpManager)

initState :: Settings -> Http.Manager -> State
initState settings httpManager
  = let
      presetA = Settings.userAPreset settings
      presetB = 100 - presetA
    in
      State
        { http           = httpManager
        , userA          = User
          { identity     = UserIdentity
            { telegramId  = ( Telegram.Username.fromString
                            . Settings.userATelegramId
                            )
              settings
            , splitwiseId = (Splitwise.UserId . Settings.userASplitwiseId)
              settings
            }
          , preset       = Split.init presetA
          , conversation = Nothing
          }
        , userB          = User
          { identity     = UserIdentity
            { telegramId  = ( Telegram.Username.fromString
                            . Settings.userBTelegramId
                            )
              settings
            , splitwiseId = (Splitwise.UserId . Settings.userBSplitwiseId)
              settings
            }
          , preset       = Split.init presetB
          , conversation = Nothing
          }
        , telegramToken  = Telegram.Token $ Settings.telegramToken settings
        , splitwiseToken = Splitwise.Token $ Settings.splitwiseToken settings
        }

loop :: Queue Message -> State -> IO ()
loop queue state = do
  msg <- Queue.dequeue queue
  case identifyUser state msg of
    Nothing -> do
      putStrLn $ "Ignoring message from unknown user" ++ show
        (Message.username msg)
      return ()
    Just session -> do
      updatedState <- processMessage state session msg
      loop queue updatedState

identifyUser :: State -> Message -> Maybe Session
identifyUser state message =
  let username    = Message.username message
      maybeUserId = matchUserId state username
  in  case maybeUserId of
        Nothing     -> Nothing
        Just userId -> Just
          (Session
            { userId = userId
            , chatId = Message.chatId message
            , user   = getUser userId state
            , buddy  = identity (getUser (otherUserId userId) state)
            }
          )

processMessage :: State -> Session -> Message -> IO State
processMessage state session message =
  let (user', effects) = reply (user session) message
      state'           = updateUser (userId session) (const user') state
  in  do
        runEffects state' (session { user = user' }) effects
        return state'

reply :: User -> Message -> (User, [Effect])
reply user message =
  let messageText                    = Message.text message
      (updatedConversation, effects) = case conversation user of
        Nothing -> Conversation.start messageText (preset user)
        Just c  -> Conversation.advance messageText c
  in  (user { conversation = updatedConversation }, effects)


runEffects :: State -> Session -> [Effect] -> IO ()
runEffects state session effects = do
  _ <- sequence (run <$> effects)
  return ()
  where run = runEffect state session

runEffect :: State -> Session -> Effect -> IO ()
runEffect state session effect =
  let httpManager = http state
  in  case effect of
        Answer reply -> Telegram.sendMessage httpManager
                                             (telegramToken state)
                                             (chatId session)
                                             reply
        Done expense -> do
          sucess <- Splitwise.createExpense
            httpManager
            (splitwiseId ((identity . user) session))
            (splitwiseId (buddy session))
            expense
            (splitwiseToken state)
          if sucess
            then Telegram.sendMessage httpManager
                                      (telegramToken state)
                                      (chatId session)
                                      (Reply.plain "Done! 🎉 💸")
            else return ()

getUser :: UserId -> State -> User
getUser userId = case userId of
  UserA -> userA
  UserB -> userB

otherUserId :: UserId -> UserId
otherUserId userId = case userId of
  UserA -> UserB
  UserB -> UserA

matchUserId :: State -> Telegram.Username.Username -> Maybe UserId
matchUserId state uname
  | (telegramId . identity . userA) state == uname = Just UserA
  | (telegramId . identity . userB) state == uname = Just UserB
  | otherwise = Nothing

updateUser :: UserId -> (User -> User) -> State -> State
updateUser userId f state = case userId of
  UserA -> state { userA = f (userA state) }
  UserB -> state { userB = f (userB state) }
