module Util where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
import           Control.Monad
import           Data.List.Compat
import qualified Data.Set as Set
import           System.Directory
import           System.FilePath

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = do
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

listDirectories :: FilePath -> IO [FilePath]
listDirectories dir = do
  files <- sort <$>
    filter (`notElem` [".", ".."]) <$>
    getDirectoryContents dir
  filterM doesDirectoryExist $ map (dir </>) files

die :: String -> String -> IO a
die file message = throwIO . ErrorCall $ file ++ ": " ++ message
