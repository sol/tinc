{-# LANGUAGE RecordWildCards #-}
module Tinc.Cabal where

import qualified Tinc.Nix as Nix
import           Tinc.Facts

cabal :: Facts -> [String] -> (String, [String])
cabal Facts{..}
  | factsUseNix = Nix.cabal
  | otherwise = (,) "cabal"
