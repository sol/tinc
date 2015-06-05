
module PackageGraphSpec where

import           Data.Graph.Wrapper
import           Data.List
import           Test.Hspec

import           PackageGraph
import           Package

spec :: Spec
spec = do
  describe "fromDot" $ do
    it "can parse dot graphs" $ do
      let Right graph = fromDot $ unlines $
            "digraph g {" :
            "  b -> c;" :
            "  a -> b;" :
            "  d;" :
            "}" :
            []
      sort (toList graph) `shouldBe`
        (Package "a" "", (), [Package "b" ""]) :
        (Package "b" "", (), [Package "c" ""]) :
        (Package "c" "", (), []) :
        (Package "d" "", (), []) :
        []
