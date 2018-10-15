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
import qualified Telegram.Username
import           Telegram.Username (Username)

data State = State
  { http :: Http.Manager
  , userA :: User
  , userB :: User
  , telegramToken :: String
  , splitwise :: Splitwise.State
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

data ChatState = ChatState
  { currentUser :: UserIdentity
  , buddy :: UserIdentity
  , chatId :: ChatId
  }

data UserId
  = UserA
  | UserB
  deriving (Show)

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
        { http          = httpManager
        , userA         = User
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
        , userB         = User
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
        , telegramToken = Settings.telegramToken settings
        , splitwise     = Splitwise.init
          $ (Splitwise.Token . Settings.splitwiseToken) settings
        }

loop :: Queue Message -> State -> IO ()
loop queue state = do
  msg          <- Queue.dequeue queue
  updatedState <- processMessage state msg
  loop queue updatedState

processMessage :: State -> Message -> IO State
processMessage state message = do
  let username    = Message.username message
      maybeUserId = matchUserId state username
  case maybeUserId of
    Nothing -> do
      putStrLn $ "Ignoring message from unknown user" ++ show username
      return state
    Just userId -> do
      reply state userId message

reply :: State -> UserId -> Message -> IO State
reply state userId message
  = let
      currentUser                    = getUser userId state

      (updatedConversation, effects) = advanceConversation
        (Message.text message)
        (preset currentUser)
        (conversation currentUser)

      state' = updateUser
        userId
        (currentUser { conversation = updatedConversation })
        state
    in
      do
        runEffects
          state'
          (ChatState
            { currentUser = identity currentUser
            , buddy       = identity (getUser (otherUserId userId) state)
            , chatId      = Message.chatId message
            }
          )
          effects
        return state'

advanceConversation
  :: String -> Split -> Maybe Conversation -> (Maybe Conversation, [Effect])
advanceConversation message preset current = case current of
  Nothing -> Conversation.start message preset
  Just c  -> Conversation.advance message c

runEffects :: State -> ChatState -> [Effect] -> IO ()
runEffects state chatState effects = do
  _ <- sequence (run <$> effects)
  return ()
  where run = runEffect state chatState

runEffect :: State -> ChatState -> Effect -> IO ()
runEffect state chatState effect =
  let httpManager    = http state
      splitwiseState = splitwise state
  in  case effect of
        Answer reply -> Telegram.sendMessage httpManager
                                             (telegramToken state)
                                             (chatId chatState)
                                             reply
        StoreAndReply expense reply -> do
          sucess <- Splitwise.createExpense
            httpManager
            (splitwiseId (currentUser chatState))
            (splitwiseId (buddy chatState))
            expense
            splitwiseState
          if sucess
            then Telegram.sendMessage httpManager
                                      (telegramToken state)
                                      (chatId chatState)
                                      reply
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

updateUser :: UserId -> User -> State -> State
updateUser userId updatedUser state = case userId of
  UserA -> state { userA = updatedUser }
  UserB -> state { userB = updatedUser }
