{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
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
, derivationFile
#endif
) where

import           Data.Char
import           Data.Maybe
import           Data.List
import           System.FilePath
import           System.Process.Internals (translate)
import           System.Process
import           System.IO

import           Tinc.Facts
import           Tinc.Package
import           Tinc.Types
import           Tinc.AddSource
import           Util

type NixExpression = String
type Argument = String
type HaskellDependency = String
type SystemDependency = String
data Function = Function {
  _functionArguments :: [Argument]
, _functionBody :: NixExpression
} deriving (Eq, Show)

packageFile :: FilePath
packageFile = "package.nix"

defaultFile :: FilePath
defaultFile = "default.nix"

shellFile :: FilePath
shellFile = "shell.nix"

cabal :: Facts -> [String] -> (String, [String])
cabal Facts{..} args = ("nix-shell", ["-p", "haskell.packages." ++ show factsNixResolver ++ ".ghcWithPackages (p: [ p.cabal-install ])", "--pure", "--run", unwords $ "cabal" : map translate args])

nixShell :: String -> [String] -> (String, [String])
nixShell command args = ("nix-shell", [shellFile, "--run", unwords $ command : map translate args])

createDerivations :: Facts -> [Package] -> IO ()
createDerivations facts@Facts{..} dependencies = do
  mapM_ (populateCache factsAddSourceCache factsNixCache) dependencies
  pkgDerivation <- cabalToNix "."

  let knownHaskellDependencies = map packageName dependencies
  derivation <- projectDerivation factsNixCache pkgDerivation <$> mapM (readDependencies factsNixCache knownHaskellDependencies) dependencies
  writeFile packageFile derivation
  writeFile defaultFile (defaultDerivation facts)
  writeFile shellFile (shellDerivation facts)

populateCache :: Path AddSourceCache -> Path NixCache -> Package -> IO ()
populateCache addSourceCache cache pkg = do
  _ <- cachedIO (derivationFile cache pkg) $ disableDocumentation . disableTests <$> go
  return ()
  where
    go = case pkg of
      Package _ (Version _ Nothing) -> cabalToNix ("cabal://" ++ showPackage pkg)
      Package name (Version _ (Just ref)) -> cabalToNix (path . addSourcePath addSourceCache $ AddSource name ref)

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

defaultDerivation :: Facts -> NixExpression
defaultDerivation Facts{..} = unlines [
    "{ nixpkgs ? import <nixpkgs> {}, compiler ? " ++ show factsNixResolver ++ " }:"
  , "nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./package.nix { }"
  ]

shellDerivation :: Facts -> NixExpression
shellDerivation Facts{..} = unlines [
    "{ nixpkgs ? import <nixpkgs> {}, compiler ? " ++ show factsNixResolver ++ " }:"
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
derivationFile cache package = path cache </> showPackage package ++ rev ++ ".nix"
  where
    rev = case packageVersion package of
      Version _ (Just hash) -> "-" ++ hash
      _ -> ""

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
