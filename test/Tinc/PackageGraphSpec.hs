module Tinc.PackageGraphSpec where

import qualified Data.Graph.Wrapper as G

import           Helper

import           Tinc.Package
import           Tinc.PackageGraph

spec :: Spec
spec = do
  describe "calculateReusablePackages" $ do
    it "finds reusable packages" $ do
      let a = Package "a" "1"
          g = G.fromList [(a, (), [])]
      calculateReusablePackages [a] g `shouldBe` [(a, ())]

    context "when a package is not part of the install plan" $ do
      it "excludes the package" $ do
        let a = Package "a" "1"
            b = Package "b" "1"
            g = G.fromList [(a, (), [])]
        calculateReusablePackages [b] g `shouldBe` []

    context "when the install plan misses a dependency" $ do
      it "excludes the package" $ do
        let a = Package "a" "1"
            b = Package "b" "1"
            g = G.fromList $
              (a, (), [b]) :
              (b, (), []) :
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
          (Package "a" "", (), [Package "b" ""]) :
          (Package "b" "", (), [Package "c" ""]) :
          (Package "c" "", (), []) :
          (Package "d" "", (), []) :
          []

        values =
          (Package "a" "", ()) :
          (Package "b" "", ()) :
          (Package "d" "", ()) :
          (Package "c" "", ()) :
          []

    it "parses dot graphs" $ do
      let graph = fromDot values dot
      graph `shouldBe` Right (G.fromList expected)

    context "when it encounters an outgoing node with a missing value" $ do
      it "returns an error" $ do
        let graph = fromDot (drop 1 values) dot
        graph `shouldBe` Left ("No value for package: " ++ show (Package "a" ""))

    context "when it encounters an ingoing node with a missing value" $ do
      it "returns an error" $ do
        let graph = fromDot (init values) dot
        graph `shouldBe` Left ("No value for package: " ++ show (Package "c" ""))
