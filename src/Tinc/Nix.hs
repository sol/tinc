{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Tinc.Nix (
  NixCache
, cabal
, nixShell
, resolverFile
, createDerivations
, cabalCompilerParams
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

import           Data.Char
import           Data.Maybe
import           Data.List
import           System.Directory
import           System.FilePath
import           System.Process.Internals (translate)
import           System.Process
import           System.IO

import           Tinc.Config
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

resolverFile :: FilePath
resolverFile = "tinc.nix"

defaultFile :: FilePath
defaultFile = "default.nix"

shellFile :: FilePath
shellFile = "shell.nix"

formatCompiler :: Maybe String -> String
formatCompiler = maybe "haskellPackages" (\c -> "haskell.packages." ++ c)

formattedCompiler :: IO String
formattedCompiler = fmap formatCompiler getCompiler

cabal :: [String] -> Maybe String -> (String, [String])
cabal args compiler = ("nix-shell", ["-p", "curl", (formatCompiler compiler) ++ ".ghcWithPackages (p: [ p.cabal-install ])", "--pure", "--run", unwords $ "cabal" : map translate args])

nixShell :: String -> [String] -> (String, [String])
nixShell command args = ("nix-shell", [shellFile, "--run", unwords $ command : map translate args])

createDerivations :: Facts -> [Package] -> IO ()
createDerivations facts@Facts{..} dependencies = do
  compiler <- getCompiler
  mapM_ (populateCache factsAddSourceCache factsNixCache) dependencies
  pkgDerivation <- cabalToNix compiler "."

  let knownHaskellDependencies = map packageName dependencies
  mapM (readDependencies factsNixCache knownHaskellDependencies) dependencies >>= resolverDerivation facts >>= writeFile resolverFile
  writeFile packageFile pkgDerivation
  writeFile defaultFile defaultDerivation
  writeFile shellFile shellDerivation

populateCache :: Path AddSourceCache -> Path NixCache -> Package -> IO ()
populateCache addSourceCache cache pkg = do
  compiler <- getCompiler
  let cacheFile = derivationFile cache pkg compiler
  createDirectoryIfMissing True $ takeDirectory cacheFile
  _ <- cachedIO cacheFile $ disableDocumentation . disableTests <$> go compiler
  return ()
  where
    go compiler = case pkg of
      Package _ (Version _ Nothing) -> cabalToNix compiler ("cabal://" ++ showPackage pkg)
      Package name (Version _ (Just ref)) -> do
        let p = path . addSourcePath addSourceCache $ AddSource name ref
        canonicalizePath p >>= cabalToNix compiler

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

cabalCompilerParams :: Maybe String -> [String]
cabalCompilerParams Nothing           = []
cabalCompilerParams (Just compiler)
  | isInfixOf "ghcjs" $ fmap toLower compiler = ["--compiler=ghcjs"]
  | otherwise                                 = []

cabalToNix :: Maybe String -> String -> IO NixExpression
cabalToNix compiler uri = do
  let params = (cabalCompilerParams compiler) ++ [uri]--["--hackage-db", homeDir </> ".cabal/packages/hackage.haskell.org/01-index.tar", uri]
  hPutStrLn stderr $ "cabal2nix " ++ (intercalate " " params)
  readProcess "cabal2nix" params ""

defaultDerivation :: NixExpression
defaultDerivation = unlines [
    "let"
  , "  default = { nixpkgs ? import <nixpkgs> {} }:"
  , "    (import ./" ++ resolverFile ++ " { inherit nixpkgs; }).resolver.callPackage ./" ++ packageFile ++ " {};"
  , "  overrideFile = ./default-override.nix;"
  , "  expr = if builtins.pathExists overrideFile then import overrideFile else default;"
  , "in expr"
  ]

shellDerivation :: NixExpression
shellDerivation = unlines [
    "{ nixpkgs ? import <nixpkgs> {} }:"
  , "(import ./" ++ defaultFile ++ " { inherit nixpkgs; }).env"
  ]

indent :: Int -> [String] -> [String]
indent n = map f
  where
    f xs = case xs of
      "" -> ""
      _ -> replicate n ' ' ++ xs

resolverDerivation :: Facts -> [(Package, [HaskellDependency], [SystemDependency])] -> IO NixExpression
resolverDerivation Facts{..} dependencies = do
  overrides <- concat <$> mapM getPkgDerivation dependencies
  compiler <- formattedCompiler
  return . unlines $ [
      "{ nixpkgs }:"
    , "rec {"
    , "  compiler = nixpkgs." ++ compiler ++ ";"
    , "  resolver ="
    ] ++ indent 4 [
      "let"
    , "  callPackage = compiler.callPackage;"
    , ""
    , "  overrideFunction = self: super: rec {"
    ] ++ indent 8 overrides ++
    indent 4 [
      "  };"
    , ""
    , "  newResolver = compiler.override {"
    , "    overrides = overrideFunction;"
    , "  };"
    , ""
    , "in newResolver;"
    ] ++ ["}"]
  where
    getPkgDerivation packageDeps@(package, _, _) = do
      compiler <- getCompiler
      let derivation = derivationFile factsNixCache package compiler
      pkgImport packageDeps <$> readFile derivation

pkgImport :: (Package, [HaskellDependency], [SystemDependency]) -> NixExpression -> [String]
pkgImport ((Package name _), _, systemDependencies) derivation = begin : indent 2 definition
  where
    begin = name ++ " = callPackage"
    derivationLines = lines derivation
    inlineDerivation = ["("] ++ indent 2 derivationLines ++ [")"]
    args = "{ " ++ inheritSystemDependencies ++ "};"
    definition = inlineDerivation ++ [args]
    inheritSystemDependencies
      | null systemDependencies = ""
      | otherwise = "inherit (nixpkgs) " ++ intercalate " " systemDependencies ++ "; "

readDependencies :: Path NixCache -> [HaskellDependency] -> Package -> IO (Package, [HaskellDependency], [SystemDependency])
readDependencies cache knownHaskellDependencies package = do
  compiler <- getCompiler
  let derivation = derivationFile cache package compiler
  (\(haskellDeps, systemDeps) -> (package, haskellDeps, systemDeps)) . (`extractDependencies` knownHaskellDependencies) . parseNixFunction <$> readFile derivation

derivationFile :: Path NixCache -> Package -> Maybe String -> FilePath
derivationFile cache package compiler =
  basePath </> showPackage package ++ rev ++ ".nix"
  where
    rootPath = path cache
    basePath = maybe rootPath (\c -> rootPath </> c) compiler
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
