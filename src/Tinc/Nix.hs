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
, defaultNixTemplate
, shellNixTemplate
, defaultTemplateFile
, shellTemplateFile
, loadTemplateFile
, writeNixFile
, resolverDerivation
, pkgImport
, parseNixFunction
, disableTests
, extractDependencies
, derivationFile
#endif
) where

import           Control.Monad.IO.Class
import           Data.Char
import           Data.Maybe
import           Data.List
import qualified Data.Text as T
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.IO as TIO
import           Data.Text.Template
import           System.Directory (doesDirectoryExist, getHomeDirectory)
import           System.FilePath
import           System.Process.Internals (translate)
import           System.Process
import           System.IO

import           Tinc.Facts
import           Tinc.Fail
import           Tinc.Package
import           Tinc.Types
import           Tinc.Sandbox
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
resolverFile = "resolver.nix"

defaultTemplateFile :: FilePath
defaultTemplateFile = "default.nix.tinc-template"

shellTemplateFile :: FilePath
shellTemplateFile = "shell.nix.tinc-template"

cabal :: Facts -> [String] -> (String, [String])
cabal Facts{..} args = ("nix-shell", ["-p", "haskell.packages." ++ show factsNixResolver ++ ".ghcWithPackages (p: [ p.cabal-install ])", "--pure", "--run", unwords $ "cabal" : map translate args])

nixShell :: String -> [String] -> (String, [String])
nixShell command args = ("nix-shell", ["--run", unwords $ command : map translate args])

createDerivations :: Facts -> [Package] -> IO ()
createDerivations facts@Facts{..} dependencies = do
  mapM_ (populateCache factsAddSourceCache factsNixCache) dependencies
  cabalToNix "." >>= writeFile packageFile

  let knownHaskellDependencies = map packageName dependencies
  mapM (readDependencies factsNixCache knownHaskellDependencies) dependencies
    >>= resolverDerivation facts
    >>= writeFile resolverFile

  homeDirTemplates <- (</> ".tinc" </> "nix") <$> getHomeDirectory >>= getTemplates
  currentDirTemplates <- getTemplates "."
  let templates = nubBy compareFileNames $ currentDirTemplates ++ homeDirTemplates ++ defaultTemplates
  mapM_ (writeNixFile facts) templates

  where
    defaultTemplates = [
        (defaultTemplateFile, defaultNixTemplate)
      , (shellTemplateFile, shellNixTemplate)
      ]
    getTemplates dir = do
      exists <- doesDirectoryExist dir
      if exists
        then toTemplates . map (dir </>) . filter (templateSuffix `isSuffixOf`) <$> getDirectoryContents dir
        else return []
    toTemplates = map (\file -> (file, loadTemplateFile file))
    templateSuffix = ".nix.tinc-template"
    compareFileNames (takeFileName . fst -> file1) (takeFileName . fst -> file2) = file1 == file2

loadTemplateFile :: (MonadIO m, Fail m) => FilePath -> m Template
loadTemplateFile templateFile = do
  input <- liftIO $ TIO.readFile templateFile
  case templateSafe input of
    Right loadedTemplate -> return loadedTemplate
    Left (row, col) -> do
      die $ "Error: " ++ templateFile ++ " has error at row " ++ show row ++ " column " ++ show col ++ "."

writeNixFile :: Facts -> (FilePath, IO Template) -> IO ()
writeNixFile Facts{..} (templateFile, getTemplate) = do
  getTemplate >>= renderToFile
  where
    nixFile = takeFileName $ dropExtension templateFile
    context (T.unpack -> "resolver") = T.pack factsNixResolver
    context _ = T.pack " "
    renderToFile loadedTemplate = TIO.writeFile nixFile $ toStrict renderedTemplate
      where renderedTemplate = render loadedTemplate context

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

nixBegin :: String
nixBegin = "{ nixpkgs ? import <nixpkgs> {}, compiler ? \"$resolver\" }:"

defaultNixTemplate :: IO Template
defaultNixTemplate = return . template . T.unlines $ map T.pack [
    nixBegin
  , "(import ./resolver.nix { inherit nixpkgs compiler; }).callPackage ./package.nix { }"
  ]

shellNixTemplate :: IO Template
shellNixTemplate = return . template . T.unlines $ map T.pack [
    nixBegin
  , "(import ./default.nix { inherit nixpkgs compiler; }).env"
  ]

indent :: Int -> [String] -> [String]
indent n = map ((replicate n ' ') ++)

resolverDerivation :: Facts -> [(Package, [HaskellDependency], [SystemDependency])] -> IO NixExpression
resolverDerivation Facts{..} dependencies = do
  overrides <- concat <$> mapM getPkgDerivation dependencies
  return . unlines $ [
      "{ nixpkgs ? import <nixpkgs> {}, compiler ? " ++ show factsNixResolver ++ " }:"
    , "let"
    , "  oldResolver = builtins.getAttr compiler nixpkgs.haskell.packages;"
    , "  callPackage = oldResolver.callPackage;"
    , ""
    , "  overrideFunction = self: super: rec {"
    ] ++ indent 4 overrides ++ [
      "  };"
    , ""
    , "  newResolver = oldResolver.override {"
    , "    overrides = overrideFunction;"
    , "  };"
    , ""
    , "in newResolver"
    ]
  where
    getPkgDerivation packageDeps@(package, _, _) = pkgImport packageDeps <$> readFile (derivationFile factsNixCache package)

pkgImport :: (Package, [HaskellDependency], [SystemDependency]) -> NixExpression -> [String]
pkgImport ((Package name _), haskellDependencies, systemDependencies) derivation = begin : indent 2 definition
  where
    begin = name ++ " = callPackage"
    derivationLines = lines derivation
    inlineDerivation = ["("] ++ indent 2 derivationLines ++ [")"]
    args = "{ " ++ inheritHaskellDependencies ++ inheritSystemDependencies ++ "};"
    definition = inlineDerivation ++ [args]
    inheritHaskellDependencies
      | null haskellDependencies = ""
      | otherwise = "inherit " ++ intercalate " " haskellDependencies ++ "; "
    inheritSystemDependencies
      | null systemDependencies = ""
      | otherwise = "inherit (nixpkgs) " ++ intercalate " " systemDependencies ++ "; "

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
