module Util where

import           Control.Monad.Catch
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.List
import           GHC.Fingerprint
import           System.Directory hiding (getDirectoryContents)
import qualified System.Directory as Directory
import           System.FilePath
import           System.Process

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

linkFile :: FilePath -> FilePath -> IO ()
linkFile src_ dst = do
  src <- canonicalizePath src_
  callProcess "ln" ["-s", src, dst]

withCurrentDirectory :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withCurrentDirectory dir action = do
  bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \ _ -> do
    liftIO $ setCurrentDirectory dir
    action

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents dir = filter (`notElem` [".", ".."]) <$> Directory.getDirectoryContents dir

listDirectoryContents :: FilePath -> IO [FilePath]
listDirectoryContents dir = sort . map (dir </>) <$> getDirectoryContents dir

getDirectories :: FilePath -> IO [FilePath]
getDirectories dir = getDirectoryContents dir >>= filterM (doesDirectoryExist . (dir </>))

listDirectories :: FilePath -> IO [FilePath]
listDirectories dir = listDirectoryContents dir >>= filterM doesDirectoryExist

listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively dir = do
  c <- listDirectoryContents dir
  subdirsFiles  <- filterM doesDirectoryExist c >>= mapM listFilesRecursively
  files <- filterM doesFileExist c
  return (files ++ concat subdirsFiles)

fingerprint :: FilePath -> IO String
fingerprint dir = withCurrentDirectory dir $ do
  files <- listFilesRecursively "."
  show . fingerprintFingerprints . sort <$> mapM fingerprintFile files
  where
    fingerprintFile :: FilePath -> IO Fingerprint
    fingerprintFile file = do
      hash <- getFileHash file
      return $ fingerprintFingerprints [hash, fingerprintString file]

cachedIO :: FilePath -> IO String -> IO String
cachedIO = cachedIOAfter (return ())

cachedIOAfter :: MonadIO m => m () -> FilePath -> m String -> m String
cachedIOAfter actionAfter file action = do
  exists <- liftIO $ doesFileExist file
  if exists
    then do
      liftIO $ readFile file
    else do
      result <- action
      liftIO $ writeFile (file ++ ".tmp") result
      liftIO $ renameFile (file ++ ".tmp") file
      actionAfter
      return result

tee :: Monad m => (a -> m ()) -> a -> m a
tee action a = action a >> return a

getCabalFiles :: FilePath -> IO [FilePath]
getCabalFiles dir = filter (not . ("." `isPrefixOf`)) . filter (".cabal" `isSuffixOf`) <$> getDirectoryContents dir

whenM :: Monad m => m Bool -> m () -> m ()
whenM  condition action = condition >>= (`when` action)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condition action = condition >>= (`unless` action)
