module LocalStore (
  Handler,
  LocalStore.init,
  LocalStore.read,
  LocalStore.write,
) where

import qualified Control.Exception
import Data.List (isPrefixOf)
import qualified System.Directory
import qualified System.FilePath.Posix as FilePath
import Text.Read (readMaybe)


newtype Handler = Handler
  {basePath :: String}


init :: String -> Handler
init basePath =
  Handler {basePath = basePath}


read :: Handler -> FilePath -> IO (Either IOError String)
read handler relativePath =
  Control.Exception.try (readFile (toAbsolutePath handler relativePath))


write :: Handler -> FilePath -> String -> IO ()
write handler relativePath contents = do
  let filePath = toAbsolutePath handler relativePath
  if isWithinBasePath handler filePath
    then do
      System.Directory.createDirectoryIfMissing True (FilePath.takeDirectory filePath)
      writeFile filePath contents
    else
      Control.Exception.throwIO
        (userError ("Attempted to write outside of base path: " ++ filePath))


toAbsolutePath :: Handler -> FilePath -> FilePath
toAbsolutePath handler relativePath =
  FilePath.joinPath [basePath handler, relativePath]


isWithinBasePath :: Handler -> FilePath -> Bool
isWithinBasePath handler absolutePath =
  FilePath.normalise (basePath handler) `isPrefixOf` FilePath.normalise absolutePath
