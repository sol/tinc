{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Mockery.Action where

import           Control.Monad.IO.Class
import           Data.List
import           Test.Hspec
import           Data.WithLocation

mock :: WithLocation (Mockable a => a -> Action a)
mock a = mockMany [a]

class Mockable a where
  type Action a
  mockMany :: WithLocation([a] -> Action a)

instance (MonadIO m, Eq a, Show a) => Mockable (a, m r) where
  type Action (a, m r) = a -> m r
  mockMany expected actual = case lookup actual expected of
    Just r -> r
    _ -> unexpectedParameters False (map fst expected) actual

instance (MonadIO m, Eq a, Show a, Eq b, Show b) => Mockable (a, b, m r) where
  type Action (a, b, m r) = (a -> b -> m r)
  mockMany options a1 b1 = case lookup actual expected of
    Just r -> r
    _ -> unexpectedParameters True (map fst expected) actual
    where
      actual = (a1, b1)
      expected = map (\(a, b, r) -> ((a, b), r)) options

instance (MonadIO m, Eq a, Show a, Eq b, Show b, Eq c, Show c) => Mockable (a, b, c, m r) where
  type Action (a, b, c, m r) = (a -> b -> c -> m r)
  mockMany options a1 b1 c1 = case lookup actual expected of
    Just r -> r
    _ -> unexpectedParameters True (map fst expected) actual
    where
      actual = (a1, b1, c1)
      expected = map (\(a, b, c, r) -> ((a, b, c), r)) options

unexpectedParameters :: WithLocation ((MonadIO m, Show a) => Bool -> [a] -> a -> m r)
unexpectedParameters pluralize expected actual = do
  liftIO . expectationFailure . unlines $ [
      message
    , expectedMessage
    , actualMessage
    ]
  return (error "Test.Mockery.Action.unexpectedParameters: This should never happen!")
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
