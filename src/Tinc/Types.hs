{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tinc.Types where

import           Data.String

data CacheDir

newtype Path a = Path {path :: FilePath}
  deriving (Eq, Ord, Show, IsString)
