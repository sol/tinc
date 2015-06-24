{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Mockery.Action where


import           Control.Exception
import           Test.HUnit.Lang
import           Control.Monad.IO.Class

class Mockable a where
  type Action a
  mock :: a -> Action a

instance (MonadIO m, Eq a, Show a) => Mockable (a, m r) where
  type Action (a, m r) = a -> m r
  mock (expected, r) actual
    | actual == expected = r
    | otherwise = unexpectedParameters False (show expected) (show actual)
instance (MonadIO m, Eq a, Show a, Eq b, Show b) => Mockable (a, b, m r) where
  type Action (a, b, m r) = (a -> b -> m r)
  mock (a0, b0, r) a1 b1
    | actual == expected = r
    | otherwise = unexpectedParameters True (show expected) (show actual)
    where
      expected = (a0, b0)
      actual = (a1, b1)

instance (MonadIO m, Eq a, Show a, Eq b, Show b, Eq c, Show c) => Mockable (a, b, c, m r) where
  type Action (a, b, c, m r) = (a -> b -> c -> m r)
  mock (a0, b0, c0, r) a1 b1 c1
    | actual == expected = r
    | otherwise = unexpectedParameters True (show expected) (show actual)
    where
      expected = (a0, b0, c0)
      actual = (a1, b1, c1)

unexpectedParameters :: MonadIO m => Bool -> String -> String -> m a
unexpectedParameters pluralize expected actual = liftIO . throwIO . HUnitFailure . unlines $ [
    message
  , "expected: " ++ expected
  , " but got: " ++ actual
  ]
  where
    message
      | pluralize = "Unexected parameters to mocked action!"
      | otherwise = "Unexected parameter to mocked action!"
