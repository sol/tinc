module Util where

import qualified Data.Set as Set

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs
