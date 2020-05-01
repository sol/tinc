{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
module Tinc.Fail where

import           Data.CallStack
import           Control.Exception

class (Functor m, Applicative m, Monad m) => Fail m where
  die :: String -> m a

  dieLoc :: HasCallStack => String -> m a
  dieLoc message = die (maybe "" ((++ ": ") . srcLocFile) location ++ message)
    where
      location :: HasCallStack => Maybe SrcLoc
      location = case reverse callStack of
        (_, loc) : _ -> Just loc
        _ -> Nothing

  bug :: HasCallStack => String -> m a
  bug message = (dieLoc . unlines) [
      message
    , "This is most likely a bug.  Please report an issue at:"
    , ""
    , "  https://github.com/sol/tinc/issues"
    ]

instance Fail IO where
  die = throwIO . ErrorCall
