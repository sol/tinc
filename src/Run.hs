module Run where

import           Tinc.Setup
import           Stack

run :: IO ()
run = do
  facts <- setup
  installDependencies (factsCache facts)
