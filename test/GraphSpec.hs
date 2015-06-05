
module GraphSpec where

import           Data.Graph.Wrapper
import           Data.List
import           Test.Hspec

import           Graph

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
        ("a", (), ["b"]) :
        ("b", (), ["c"]) :
        ("c", (), []) :
        ("d", (), []) :
        []
