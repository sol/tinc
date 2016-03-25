{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Mockery.Action (
  Dummy (..)
, dummy
, dummy_
, Stub (..)
, ExpectCall (..)
) where

import           Control.Monad.IO.Class
import           Control.Monad
import           Data.IORef
import           Data.List
import           Test.Hspec
import           Data.WithLocation

failure :: WithLocation (String -> IO a)
failure err = expectationFailure err >> return undefined

dummy :: WithLocation (Dummy a => String -> a)
dummy = dummyNamed . Just

dummy_ :: WithLocation (Dummy a => a)
dummy_ = dummyNamed Nothing

class Dummy a where
  dummyNamed :: WithLocation (Maybe String -> a)

instance Dummy (IO r) where
  dummyNamed name = dummyNamed name ()

instance Dummy (a -> IO r) where
  dummyNamed name = dummyNamed name ()

instance Dummy (a -> b -> IO r) where
  dummyNamed name = dummyNamed name ()

instance Dummy (a -> b -> c -> IO r) where
  dummyNamed name = dummyNamed name ()

instance Dummy (a -> b -> c -> d -> IO r) where
  dummyNamed name = dummyNamed name ()

instance Dummy (a -> b -> c -> d -> e -> IO r) where
  dummyNamed name = dummyNamed name ()

instance Dummy (a -> b -> c -> d -> e -> f -> IO r) where
  dummyNamed name _ _ _ _ _ _ = do
    let err = "Unexpected call to dummy action" ++ maybe "!" (": " ++) name
    failure err

class Stub a where
  type Action_ a
  stub :: WithLocation(a -> Action_ a)

instance (MonadIO m, Eq a, Show a) => Stub (a, m r) where
  type Action_ (a, m r) = (a -> m r)
  stub option = stub [option]

instance (MonadIO m, Eq a, Show a, Eq b, Show b) => Stub (a, b, m r) where
  type Action_ (a, b, m r) = (a -> b -> m r)
  stub option = stub [option]

instance (MonadIO m, Eq a, Show a, Eq b, Show b, Eq c, Show c) => Stub (a, b, c, m r) where
  type Action_ (a, b, c, m r) = (a -> b -> c -> m r)
  stub option = stub [option]

instance (MonadIO m, Eq a, Show a) => Stub [(a, m r)] where
  type Action_ [(a, m r)] = a -> m r
  stub expected actual = case lookup actual expected of
    Just r -> r
    _ -> unexpectedParameters False (map fst expected) actual

instance (MonadIO m, Eq a, Show a, Eq b, Show b) => Stub [(a, b, m r)] where
  type Action_ [(a, b, m r)] = (a -> b -> m r)
  stub options a1 b1 = case lookup actual expected of
    Just r -> r
    _ -> unexpectedParameters True (map fst expected) actual
    where
      actual = (a1, b1)
      expected = map (\(a, b, r) -> ((a, b), r)) options

instance (MonadIO m, Eq a, Show a, Eq b, Show b, Eq c, Show c) => Stub [(a, b, c, m r)] where
  type Action_ [(a, b, c, m r)] = (a -> b -> c -> m r)
  stub options a1 b1 c1 = case lookup actual expected of
    Just r -> r
    _ -> unexpectedParameters True (map fst expected) actual
    where
      actual = (a1, b1, c1)
      expected = map (\(a, b, c, r) -> ((a, b, c), r)) options

unexpectedParameters :: WithLocation ((MonadIO m, Show a) => Bool -> [a] -> a -> m r)
unexpectedParameters pluralize expected actual = do
  liftIO . failure . unlines $ [
      message
    , expectedMessage
    , actualMessage
    ]
  where
    message
      | pluralize = "Unexected parameters to mocked action!"
      | otherwise = "Unexected parameter to mocked action!"

    expectedMessage = case expected of
      [x] -> "expected: " ++ show x
      _ -> "expected one of: " ++ (intercalate ", " $ map show expected)

    actualMessage = case expected of
      [_] -> " but got: " ++ show actual
      _ -> "        but got: " ++ show actual

class ExpectCall a where
  expectOnce :: WithLocation(a -> (a -> IO x) -> IO x)

instance ExpectCall (a -> IO r) where
  expectOnce action inner = expectOnce (\() -> action) $ inner . ($ ())

instance ExpectCall (a -> b -> IO r) where
  expectOnce action inner = expectOnce (\() -> action) $ inner . ($ ())

instance ExpectCall (a -> b -> c -> IO r) where
  expectOnce action inner = expectOnce (\() -> action) $ inner . ($ ())

instance ExpectCall (a -> b -> c -> d -> IO r) where
  expectOnce action inner = expectOnce (\() -> action) $ inner . ($ ())

instance ExpectCall (a -> b -> c -> d -> e -> IO r) where
  expectOnce action inner = expectOnce (\() -> action) $ inner . ($ ())

instance ExpectCall (a -> b -> c -> d -> e -> f -> IO r) where
  expectOnce action inner = do
    ref <- newIORef (0 :: Integer)
    let wrapped a b c d e f = action a b c d e f <* modifyIORef ref succ
    inner wrapped <* do
      n <- readIORef ref
      unless (n == 1) $ do
        failure ("Expected to be called once, but it was called " ++ show n ++ " times instead!")
