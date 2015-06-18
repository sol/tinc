module Main (main) where

import           Test.Hspec

import           Run (unsetEnvVars)
import qualified All

main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll_ unsetEnvVars All.spec
