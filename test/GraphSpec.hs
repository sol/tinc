
module GraphSpec where

import           Data.Graph.Wrapper
import           Data.List
import           Test.Hspec

import           Graph

spec = do
  describe "fromDot" $ do
    it "can parse dot graphs" $ do
      let dot = unlines $
            "digraph g {" :
            "  b -> c;" :
            "  a -> b;" :
            "  d;" :
            "}" :
            []
      sort (toList (fromDot dot)) `shouldBe`
        ("a", "a", ["b"]) :
        ("b", "b", ["c"]) :
        ("c", "c", []) :
        ("d", "d", []) :
        []
