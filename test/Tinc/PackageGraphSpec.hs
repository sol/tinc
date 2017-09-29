{-# LANGUAGE OverloadedStrings #-}
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
          (SimplePackage "a" "", (), [SimplePackage "b" ""]) :
          (SimplePackage "b" "", (), [SimplePackage "c" ""]) :
          (SimplePackage "c" "", (), []) :
          (SimplePackage "d" "", (), []) :
          []

        values =
          (SimplePackage "a" "", ()) :
          (SimplePackage "b" "", ()) :
          (SimplePackage "d" "", ()) :
          (SimplePackage "c" "", ()) :
          []

    it "parses dot graphs" $ do
      fromDot values dot `shouldReturn` G.fromList expected

    context "when it encounters an outgoing node with a missing value" $ do
      it "returns an error" $ do
        fromDot (drop 1 values) dot `shouldThrow`
          errorCall ("src/Tinc/PackageGraph.hs: No value for package: " ++ show (SimplePackage "a" ""))

    context "when it encounters an ingoing node with a missing value" $ do
      it "returns an error" $ do
        fromDot (init values) dot `shouldThrow`
          errorCall ("src/Tinc/PackageGraph.hs: No value for package: " ++ show (SimplePackage "c" ""))
