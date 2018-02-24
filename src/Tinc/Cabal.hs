{-# LANGUAGE RecordWildCards #-}
module Tinc.Cabal where

import qualified Tinc.Nix as Nix
import           Tinc.Facts

cabal :: Facts -> [String] -> Maybe String -> (String, [String])
cabal Facts{..} args compiler
  | factsUseNix = Nix.cabal args compiler
  | otherwise   = ("cabal", Nix.cabalCompilerParams compiler ++ args)
