{-# LANGUAGE RecordWildCards #-}
module Tinc.Cabal where

import qualified Tinc.Nix as Nix
import           Tinc.Facts

cabal :: Facts -> [String] -> (String, [String])
cabal facts@Facts{..}
  | factsUseNix = Nix.cabal facts
  | otherwise = (,) "cabal"
