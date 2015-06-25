module Tinc.Fail where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception

class (Functor m, Applicative m, Monad m) => Fail m where
  die :: String -> m a

  dieLoc :: Fail m => String -> String -> m a
  dieLoc loc message = die $ loc ++ ": " ++ message

instance Fail IO where
  die = throwIO . ErrorCall
