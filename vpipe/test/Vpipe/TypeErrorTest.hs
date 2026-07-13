{-# LANGUAGE ScopedTypeVariables #-}

module Vpipe.TypeErrorTest (typeErrorTests) where

import Control.Exception (IOException, try)
import Control.Monad (filterM, unless, when)
import Data.Char (isSpace)
import Data.List (isInfixOf, isSuffixOf, sort, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (
  canonicalizePath,
  doesDirectoryExist,
  doesFileExist,
  getCurrentDirectory,
  listDirectory,
 )
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

data Roots = Roots
  { projectRoot :: FilePath
  , packageRoot :: FilePath
  }

typeErrorTests :: TestTree
typeErrorTests = testCase "all compile-fail fixtures emit their expected diagnostics" $ do
  roots <- locateRoots
  fixtures <- discoverFixtures (packageRoot roots </> "test" </> "type-errors")
  when (null fixtures) $ assertFailure "no compile-fail fixtures were discovered"
  failures <- concat <$> mapM (checkFixture roots) fixtures
  unless (null failures) $ assertFailure (unlines failures)

locateRoots :: IO Roots
locateRoots = do
  workingDirectory <- getCurrentDirectory
  executable <- getExecutablePath
  fromWorkingDirectory <- findPackageRoot workingDirectory
  package <- case fromWorkingDirectory of
    Just path -> pure path
    Nothing -> do
      fromExecutable <- findPackageRoot (takeDirectory executable)
      maybe (assertFailure "could not locate vpipe.cabal and test/type-errors") pure fromExecutable
  project <- findProjectRoot package
  Roots <$> canonicalizePath project <*> canonicalizePath package

findPackageRoot :: FilePath -> IO (Maybe FilePath)
findPackageRoot start = canonicalizePath start >>= ascend candidate
 where
  candidate directory = do
    direct <- isPackageRoot directory
    if direct
      then pure (Just directory)
      else do
        let nested = directory </> "vpipe"
        nestedPackage <- isPackageRoot nested
        pure (if nestedPackage then Just nested else Nothing)

isPackageRoot :: FilePath -> IO Bool
isPackageRoot directory = do
  manifestExists <- doesFileExist (directory </> "vpipe.cabal")
  fixturesExist <- doesDirectoryExist (directory </> "test" </> "type-errors")
  pure (manifestExists && fixturesExist)

findProjectRoot :: FilePath -> IO FilePath
findProjectRoot package = do
  found <- ascend projectMarker package
  pure (fromMaybe package found)
 where
  projectMarker directory = do
    exists <- doesFileExist (directory </> "cabal.project")
    pure (if exists then Just directory else Nothing)

ascend :: (FilePath -> IO (Maybe a)) -> FilePath -> IO (Maybe a)
ascend inspect directory = do
  result <- inspect directory
  case result of
    Just value -> pure (Just value)
    Nothing ->
      let parent = takeDirectory directory
       in if parent == directory then pure Nothing else ascend inspect parent

discoverFixtures :: FilePath -> IO [FilePath]
discoverFixtures directory = do
  entries <- sort <$> listDirectory directory
  let paths = map (directory </>) entries
  directories <- filterM doesDirectoryExist paths
  nested <- concat <$> mapM discoverFixtures directories
  let haskellFiles = filter (".hs" `isSuffixOf`) paths
  pure (haskellFiles <> nested)

checkFixture :: Roots -> FilePath -> IO [String]
checkFixture roots fixture = do
  source <- readFile fixture
  let expected = mapMaybe expectation (lines source)
      displayPath = makeRelative (projectRoot roots) fixture
  if null expected
    then pure [displayPath <> ": contains no -- EXPECT: diagnostic"]
    else do
      processSpec <- compilerProcess roots fixture
      result <- try (readCreateProcessWithExitCode processSpec "")
      case result of
        Left (exception :: IOException) -> pure [displayPath <> ": could not invoke GHC: " <> show exception]
        Right (exitCode, standardOutput, standardError) ->
          pure (validateResult displayPath expected exitCode (standardOutput <> standardError))

compilerProcess :: Roots -> FilePath -> IO CreateProcess
compilerProcess roots fixture = do
  environment <- lookupEnv "GHC_ENVIRONMENT"
  let ghcArguments = ["-v0", "-fno-code", "-fforce-recomp", "-package", "vpipe", fixture]
      command = case environment of
        Just packageEnvironment
          | not (null packageEnvironment) && packageEnvironment /= "-" ->
              proc "ghc" ("-package-env" : packageEnvironment : ghcArguments)
        _ -> proc "cabal" ("exec" : "--" : "ghc" : ghcArguments)
  pure command{cwd = Just (projectRoot roots)}

validateResult :: FilePath -> [String] -> ExitCode -> String -> [String]
validateResult displayPath expected exitCode diagnostics = case exitFailure <> missingDiagnostics of
  [] -> []
  failures -> failures <> [displayPath <> ": compiler diagnostics:\n" <> diagnostics]
 where
  exitFailure = case exitCode of
    ExitSuccess -> [displayPath <> ": unexpectedly compiled successfully"]
    ExitFailure _ -> []
  missingDiagnostics =
    [ displayPath <> ": missing expected diagnostic: " <> show expectedDiagnostic
    | expectedDiagnostic <- expected
    , not (expectedDiagnostic `isInfixOf` diagnostics)
    ]

expectation :: String -> Maybe String
expectation sourceLine = do
  remainder <- stripPrefix "-- EXPECT:" (dropWhile isSpace sourceLine)
  let expected = trim remainder
  if null expected then Nothing else Just expected

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
