
module PackageGraphSpec where

import           Test.Hspec

import           PackageGraph
import           Package

spec :: Spec
spec = do
  describe "calculateReusablePackages" $ do
    it "finds reusable packages" $ do
      let a = Package "a" "1"
          g = toGraph [(a, [])]
      calculateReusablePackages [a] g `shouldBe` [a]

    context "when a package is not part of the install plan" $ do
      it "excludes the package" $ do
        let a = Package "a" "1"
            b = Package "b" "1"
            g = toGraph [(a, [])]
        calculateReusablePackages [b] g `shouldBe` []

    context "when the install plan misses a dependency" $ do
      it "excludes the package" $ do
        let a = Package "a" "1"
            b = Package "b" "1"
            g = toGraph $
              (a, [b]) :
              (b, []) :
              []
        calculateReusablePackages [a] g `shouldBe` []

  describe "fromDot" $ do
    let dot = unlines $
          "digraph g {" :
          "  b -> c;" :
          "  a -> b;" :
          "  d;" :
          "}" :
          []
        expected =
          (Package "a" "", [Package "b" ""]) :
          (Package "b" "", [Package "c" ""]) :
          (Package "c" "", []) :
          (Package "d" "", []) :
          []
    it "can parse dot graphs" $ do
      let Right graph = fromDot [] dot
      graph `shouldBe` toGraph expected

    it "accepts an additional list of nodes" $ do
      let Right graph = fromDot [Package "a" "", Package "e" ""] dot
      graph `shouldBe` toGraph ((Package "e" "", []) : expected)
