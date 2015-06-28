module Util where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Compat
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Data.Char
import           Data.List.Compat
import qualified Data.Set as Set
import           System.Directory
import           System.FilePath

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs

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
