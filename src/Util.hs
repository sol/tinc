module Util where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Catch
import           Control.Monad.Compat
import           Control.Monad.IO.Class
import           Data.Char
import           Data.List.Compat
import           GHC.Fingerprint
import           System.Directory
import           System.FilePath

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

withCurrentDirectory :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withCurrentDirectory dir action = do
  bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \ _ -> do
    liftIO $ setCurrentDirectory dir
    action

listDirectories :: FilePath -> IO [FilePath]
listDirectories dir = do
  files <- sort <$>
    filter (`notElem` [".", ".."]) <$>
    getDirectoryContents dir
  filterM doesDirectoryExist $ map (dir </>) files

listFiles :: FilePath -> IO [FilePath]
listFiles dir = do
  c <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
  subdirsFiles  <- filterM doesDirectoryExist c >>= mapM listFiles
  files <- filterM doesFileExist c
  return (files ++ concat subdirsFiles)

fingerprint :: FilePath -> IO String
fingerprint dir = withCurrentDirectory dir $ do
  files <- listFiles "."
  show . fingerprintFingerprints . sort <$> mapM fingerprintFile files
  where
    fingerprintFile :: FilePath -> IO Fingerprint
    fingerprintFile file = do
      hash <- getFileHash file
      return $ fingerprintFingerprints [hash, fingerprintString file]
