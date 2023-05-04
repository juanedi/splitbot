module LocalDB (
  Handler,
  LocalDB.init,
  readChatId,
  storeChatId,
) where

import qualified Control.Exception
import Core (UserId)
import qualified Core
import qualified System.Directory
import qualified System.FilePath.Posix as FilePath
import qualified Telegram.Api
import Text.Read (readMaybe)


newtype Handler = Handler
  {storePath :: String}


init :: String -> Handler
init storePath =
  Handler {storePath = storePath}


readChatId :: Handler -> UserId -> IO (Maybe Telegram.Api.ChatId)
readChatId handler userId =
  let (_, filePath) = chatIdPath (storePath handler) userId
   in do
        readResult <- tryReadFile filePath
        return
          ( case readResult of
              Left _err -> Nothing
              Right contents -> fmap Telegram.Api.ChatId (readMaybe contents)
          )


storeChatId :: Handler -> UserId -> Telegram.Api.ChatId -> IO ()
storeChatId handler userId (Telegram.Api.ChatId chatId) =
  let (directoryPath, filePath) = chatIdPath (storePath handler) userId
   in do
        System.Directory.createDirectoryIfMissing True directoryPath
        writeFile filePath (show chatId)


tryReadFile :: FilePath -> IO (Either IOError String)
tryReadFile path = Control.Exception.try (readFile path)


chatIdPath :: FilePath -> Core.UserId -> (FilePath, FilePath)
chatIdPath storePath userId =
  let userIdPart =
        case userId of
          Core.UserA -> "a"
          Core.UserB -> "b"
      directoryPath =
        FilePath.joinPath [storePath, "users", userIdPart]
   in (directoryPath, FilePath.joinPath [directoryPath, "chat_id"])
