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
, resolverDerivation
, pkgImport
, parseNixFunction
, disableTests
, extractDependencies
, derivationFile
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
import           Tinc.Sandbox
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

resolverFile :: FilePath
resolverFile = "resolver.nix"

defaultFile :: FilePath
defaultFile = "default.nix"

shellFile :: FilePath
shellFile = "shell.nix"

cabal :: [String] -> (String, [String])
cabal args = ("nix-shell", ["-p", "haskell.packages." ++ show defaultResolver ++ ".ghcWithPackages (p: [ ])", "--run", unwords $ "cabal" : map translate args])

nixShell :: String -> [String] -> (String, [String])
nixShell command args = ("nix-shell", [shellFile, "--run", unwords $ command : map translate args])

createDerivations :: Path AddSourceCache -> Path NixCache -> [Package] -> IO ()
createDerivations addSourceCache cache dependencies = do
  mapM_ (populateCache addSourceCache cache) dependencies
  pkgDerivation <- cabalToNix "."

  let knownHaskellDependencies = map packageName dependencies
  resDerivation <- resolverDerivation cache <$> mapM (readDependencies cache knownHaskellDependencies) dependencies
  writeFile packageFile pkgDerivation
  writeFile resolverFile resDerivation
  writeFile defaultFile defaultDerivation
  writeFile shellFile shellDerivation

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

defaultDerivation :: NixExpression
defaultDerivation = unlines [
    "let"
  , "  config = if builtins.pathExists ./config.nix then import ./config.nix else { };"
  , "  defaultConfig = attr: default: if builtins.hasAttr attr config then builtins.getAttr attr config else default;"
  , "  compiler_ = defaultConfig \"compiler\" \"ghc7103\";"
  , "in"
  , "{ compiler ? compiler_ }:"
  , "(import ./resolver.nix { inherit compiler; }).callPackage ./package.nix { }"
  ]

shellDerivation :: NixExpression
shellDerivation = unlines [
    "let"
  , "  config = if builtins.pathExists ./config.nix then import ./config.nix else { };"
  , "  defaultConfig = attr: default: if builtins.hasAttr attr config then builtins.getAttr attr config else default;"
  , "  compiler_ = defaultConfig \"compiler\" \"ghc7103\";"
  , "in"
  , "{ compiler ? compiler_ }:"
  , "(import ./default.nix { inherit compiler; }).env"
  ]

resolverDerivation :: Path NixCache -> [(Package, [HaskellDependency], [SystemDependency])] -> NixExpression
resolverDerivation cache dependencies = unlines $ [
    "let"
  , "  defaultConfig = attr: default: if builtins.hasAttr attr config then builtins.getAttr attr config else default;"
  , "  config = if builtins.pathExists ./config.nix then import ./config.nix else { };"
  , "  compiler_ = defaultConfig \"compiler\" \"ghc7103\";"
  , "in"
  , "{ compiler ? compiler_ }:"
  , "let"
  , "  pkgs = import <nixpkgs> {};"
  , "  oldResolver = builtins.getAttr compiler pkgs.haskell.packages;"
  , "  callPackage = oldResolver.callPackage;"
  , ""
  , "  overrideFunction = self: super: rec {"
  ] ++ indent overrides ++ [
    "  };"
  , ""
  , "  newResolver = oldResolver.override {"
  , "    overrides = overrideFunction;"
  , "  };"
  , ""
  , "in newResolver"
  ]
  where
    overrides = map (pkgImport cache) dependencies
    indent = map ("    " ++)

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
