{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tinc.Types where

import           Data.String

newtype Path a = Path {path :: FilePath}
  deriving (Eq, Ord, Show, IsString)
