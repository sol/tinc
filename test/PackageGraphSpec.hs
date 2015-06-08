
module PackageGraphSpec where

import           Data.Graph.Wrapper
import           Data.List
import           Test.Hspec

import           PackageGraph
import           Package

spec :: Spec
spec = do
  describe "reusablePackages" $ do
    it "finds reusable packages" $ do
      let a = Package "a" "1"
          g = toGraph [(a, [])]
      reusablePackages [a] g `shouldBe` [a]

    context "when a package is not part of the install plan" $ do
      it "excludes the package" $ do
        let a = Package "a" "1"
            b = Package "b" "1"
            g = toGraph [(a, [])]
        reusablePackages [b] g `shouldBe` []

    context "when the install plan misses a dependency" $ do
      it "excludes the package" $ do
        let a = Package "a" "1"
            b = Package "b" "1"
            g = toGraph $
              (a, [b]) :
              (b, []) :
              []
        reusablePackages [a] g `shouldBe` []

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
