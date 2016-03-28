{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
module Tinc.Nix (
  NixCache
, cabal
, nixShell
, createDerivations
#ifdef TEST
, Function (..)
, defaultDerivation
, shellDerivation
, projectDerivation
, pkgImport
, parseNixFunction
, disableTests
, extractDependencies
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Data.Char
import           Data.Maybe
import           Data.List.Compat
import           System.FilePath
import           System.Process.Internals (translate)
import           System.Process
import           System.IO

import           Tinc.Package
import           Tinc.Types
import           Util

data NixCache
type NixExpression = String
type Argument = String
type HaskellDependency = String
type SystemDependency = String
data Function = Function {
  _functionArguments :: [Argument]
, _functionBody :: NixExpression
} deriving (Eq, Show)

defaultResolver :: String
defaultResolver = "ghc7103"

packageFile :: FilePath
packageFile = "package.nix"

defaultFile :: FilePath
defaultFile = "default.nix"

shellFile :: FilePath
shellFile = "shell.nix"

cabal :: [String] -> (String, [String])
cabal args = ("nix-shell", ["-p", "haskell.packages." ++ show defaultResolver ++ ".ghcWithPackages (p: [ ])", "--run", unwords $ "cabal" : map translate args])

nixShell :: String -> [String] -> (String, [String])
nixShell command args = ("nix-shell", [shellFile, "--run", unwords $ command : map translate args])

createDerivations :: Path NixCache -> [Package] -> IO ()
createDerivations cache dependencies = do
  mapM_ (populateCache cache) dependencies
  pkgDerivation <- cabalToNix "."

  let knownHaskellDependencies = map packageName dependencies
  derivation <- projectDerivation cache pkgDerivation <$> mapM (readDependencies cache knownHaskellDependencies) dependencies
  writeFile packageFile derivation
  writeFile defaultFile defaultDerivation
  writeFile shellFile shellDerivation

populateCache :: Path NixCache -> Package -> IO ()
populateCache cache pkg = do
  _ <- cachedIO (derivationFile cache pkg) $ disableDocumentation . disableTests <$> cabalToNix ("cabal://" ++ showPackage pkg)
  return ()

disable :: String -> NixExpression -> NixExpression
disable s xs = case lines xs of
  ys | attribute `elem` ys -> xs
  ys -> unlines . (++ [attribute, "}"]) . init $ ys
  where
    attribute = "  " ++ s ++ " = false;"

disableTests :: NixExpression -> NixExpression
disableTests = disable "doCheck"

disableDocumentation :: NixExpression -> NixExpression
disableDocumentation = disable "doHaddock"

cabalToNix :: String -> IO NixExpression
cabalToNix uri = do
  hPutStrLn stderr $ "cabal2nix " ++ uri
  readProcess "cabal2nix" [uri] ""

defaultDerivation :: NixExpression
defaultDerivation = unlines [
    "{ nixpkgs ? import <nixpkgs> {}, compiler ? " ++ show defaultResolver ++ " }:"
  , "nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./package.nix { }"
  ]

shellDerivation :: NixExpression
shellDerivation = unlines [
    "{ nixpkgs ? import <nixpkgs> {}, compiler ? " ++ show defaultResolver ++ " }:"
  , "(import ./default.nix { inherit nixpkgs compiler; }).env"
  ]

projectDerivation :: Path NixCache -> NixExpression -> [(Package, [HaskellDependency], [SystemDependency])] -> NixExpression
projectDerivation cache pkgDerivation dependencies = renderNixFunction pkg
  where
    function = parseNixFunction pkgDerivation
    knownHaskellDependencies = map (packageName . (\(p, _, _) -> p)) dependencies
    addLetBindings body = unlines $ "let" : indent pkgImports ++ ["in " ++ body]
    nixPkgImport = "pkgs = (import <nixpkgs> {}).pkgs;"
    pkgImports = nixPkgImport : map (pkgImport cache) dependencies
    indent = map ("  " ++)
    pkg = case function of
      Function args body -> Function ("callPackage" : filter (`notElem` knownHaskellDependencies) args) $ addLetBindings body

renderNixFunction :: Function -> NixExpression
renderNixFunction (Function args body) = "{ " ++ intercalate ", " args ++ " }:\n" ++ body

pkgImport :: Path NixCache -> (Package, [HaskellDependency], [SystemDependency]) -> String
pkgImport cache (package@(Package name _), haskellDependencies, systemDependencies) =
  name ++ " = callPackage " ++ derivationFile cache package ++ " " ++ args ++ ";"
  where
    args = "{ " ++ inheritHaskellDependencies ++ inheritSystemDependencies ++ "}"
    inheritHaskellDependencies
      | null haskellDependencies = ""
      | otherwise = "inherit " ++ intercalate " " haskellDependencies ++ "; "
    inheritSystemDependencies
      | null systemDependencies = ""
      | otherwise = "inherit (pkgs) " ++ intercalate " " systemDependencies ++ "; "

readDependencies :: Path NixCache -> [HaskellDependency] -> Package -> IO (Package, [HaskellDependency], [SystemDependency])
readDependencies cache knownHaskellDependencies package = do
  (\(haskellDeps, systemDeps) -> (package, haskellDeps, systemDeps)) . (`extractDependencies` knownHaskellDependencies)  . parseNixFunction <$> readFile (derivationFile cache package)

derivationFile :: Path NixCache -> Package -> FilePath
derivationFile cache package = path cache </> showPackage package ++ ".nix"

parseNixFunction :: NixExpression -> Function
parseNixFunction xs = case break (== '}') xs of
  (args, body) -> Function (split . filter (not . isSpace). dropWhile (`elem` "{ ") $ args) (dropWhile (`elem` "}: ") body)
  where
    split :: String -> [Argument]
    split = go ""
      where
        go acc ys = case ys of
          ',' : zs -> reverse acc : go "" zs
          z : zs -> go (z : acc) zs
          "" -> [reverse acc]

extractDependencies :: Function -> [HaskellDependency] -> ([HaskellDependency], [SystemDependency])
extractDependencies (Function args body) knownHaskellDependencies =
  (haskellDependencies, systemDependencies)
  where
    haskellDependencies = filter (`notElem` systemDependencies) . filter (`elem` knownHaskellDependencies) $ args
    systemDependencies = parseSystemDependencies body

parseSystemDependencies :: NixExpression -> [SystemDependency]
parseSystemDependencies body = concatMap (words . takeWhile (/= ']')) . mapMaybe (stripPrefix "librarySystemDepends = [" . dropWhile isSpace) . lines $ body
