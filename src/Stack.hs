
module Stack where

import           Control.Exception
import           Data.List
import           Data.Maybe
import           Data.Traversable
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

import           Graph

createStackedSandbox :: Path SandboxParent -> IO ()
createStackedSandbox source = do
  callCommand "cabal sandbox init"
  destPackageDB <- findPackageDB (Path "." :: Path SandboxParent)
  sourcePackageDB <- findPackageDB source
  packages <- extractPackages sourcePackageDB
  mapM_ (registerPackage destPackageDB) packages

findPackageDB :: Path SandboxParent -> IO (Path PackageDB)
findPackageDB dir = do
  mr <-
    listToMaybe <$>
    filter ("-packages.conf.d" `isSuffixOf`) <$>
    getDirectoryContents (path dir </> ".cabal-sandbox")
  maybe
    (die ("package db not found in " ++ path dir))
    (\ p -> Path <$> canonicalizePath (path dir </> ".cabal-sandbox" </> p))
    mr

extractPackages :: Path PackageDB -> IO [Path Package]
extractPackages packageDB = do
  packageFiles <-
    filter (".conf" `isSuffixOf`) <$>
    getDirectoryContents (path packageDB)
  globalPackageDB <- findGlobalPackageDB
  dot <- readProcess "ghc-pkg"
    ("--package-db" : path globalPackageDB :
     "--package-db" : path packageDB :
     "dot" : []) []
  packagesInTopologicalOrder <- do
    case fromDot dot of
      Right graph ->
        return $ reverse $ topolocicalOrder graph
      Left message -> die message
  catMaybes <$> forM packagesInTopologicalOrder
    (\ packageFromGraph ->
      case filter (packageFromGraph `isPrefixOf`) packageFiles of
        [packageFile] -> return $
          Just $ Path (path packageDB </> packageFile)
        [] -> return Nothing
        multiple -> die ("package found multiple times: " ++ unwords multiple))

findGlobalPackageDB :: IO (Path PackageDB)
findGlobalPackageDB = do
  output <- readProcess "ghc-pkg" ["list"] []
  return $ Path $ takeWhile (/= ':') output

registerPackage :: Path PackageDB -> Path Package -> IO ()
registerPackage packageDB package =
  callCommand ("ghc-pkg register --package-db=" ++ path packageDB ++ " " ++ path package)

-- * indexed paths

data Path a
  = Path {path :: FilePath}
  deriving (Eq, Show)

data SandboxParent
data PackageDB
data Package

-- * utils

withDirectory :: FilePath -> IO a -> IO a
withDirectory dir action = bracket start stop (const action)
 where
  start = do
    outer <- getCurrentDirectory
    createDirectoryIfMissing True dir
    setCurrentDirectory dir
    return outer
  stop = setCurrentDirectory

