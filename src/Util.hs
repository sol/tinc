module Util where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
import           Control.Monad
import           Data.List.Compat
import qualified Data.Set as Set
import           System.Directory
import           System.FilePath

import           System.Clock

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

measure :: String -> IO a -> IO a
measure name action = do
  t0 <- getTime Monotonic
  a <- action
  t1 <- getTime Monotonic
  putStrLn (name ++ " " ++ show (timeSpecAsNanoSecs (t1 - t0) `div` 1000000) ++ "ms")
  return a
