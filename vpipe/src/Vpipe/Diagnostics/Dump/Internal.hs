{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Best-effort compiler artifact dumping controlled by @VPIPE_DUMP@.
module Vpipe.Diagnostics.Dump.Internal (
  ShaderDumpStage (..),
  ShaderDump (..),
  ShaderFailureKind (..),
  classifyShaderFailure,
  dumpCompiledModule,
  dumpCompiledModuleWith,
  retainShaderFailureArtifact,
  retainShaderFailureArtifactWith,
  throwShaderCompileBugWith,
  throwShaderDriverFailureWith,
  renderInterfaceTable,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (AsyncException, IOException, SomeException, catch, fromException, mask, onException, throwIO, try)
import Control.Monad (void)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.Either (fromRight)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, getTemporaryDirectory, removeFile, renameFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (Handle, hClose, hFlush, openBinaryTempFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import Vulkan.Core10.Enums.Result qualified as Result

import Vpipe.Error (VpipeError (DeviceLost, ShaderCompileBug, VulkanFailure))
import Vpipe.SpirV.Assembler (SpirVModule, moduleBytes)

data ShaderDumpStage
  = DumpCompute
  | DumpVertex
  | DumpFragment
  deriving stock (Eq, Show)

data ShaderDump = ShaderDump
  { shaderDumpName :: String
  , shaderDumpStage :: ShaderDumpStage
  , shaderDumpModule :: SpirVModule
  , shaderDumpInterface :: String
  }

data ShaderFailureKind
  = GeneratedShaderRejected
  | ShaderDeviceLost
  | OtherShaderFailure
  deriving stock (Eq, Show)

classifyShaderFailure :: Result.Result -> ShaderFailureKind
classifyShaderFailure result
  | result == Result.ERROR_INVALID_SHADER_NV = GeneratedShaderRejected
  | result == Result.ERROR_DEVICE_LOST = ShaderDeviceLost
  | otherwise = OtherShaderFailure

{- | Dump through the process environment. If @VPIPE_DUMP@ is unset or empty,
this performs no filesystem lookup or process discovery.
-}
dumpCompiledModule :: ShaderDump -> IO ()
dumpCompiledModule =
  void
    . dumpCompiledModuleWith
      (lookupEnv "VPIPE_DUMP")
      (findExecutable "spirv-dis")

{- | Injectable environment and tool discovery seam used by focused tests.
All synchronous failures are deliberately swallowed: diagnostics must never
turn a successful shader compilation into a failure.
-}
dumpCompiledModuleWith :: IO (Maybe FilePath) -> IO (Maybe FilePath) -> ShaderDump -> IO (Maybe FilePath)
dumpCompiledModuleWith lookupDirectory findDisassembler request =
  bestEffort $ do
    configured <- lookupDirectory
    case configured of
      Just directory | not (null directory) ->
        withMVar dumpLock $ \_ -> dumpToDirectory directory findDisassembler request
      _ -> pure Nothing

dumpToDirectory :: FilePath -> IO (Maybe FilePath) -> ShaderDump -> IO (Maybe FilePath)
dumpToDirectory directory findDisassembler request = do
  createDirectoryIfMissing True directory
  let spirV = LazyByteString.toStrict (moduleBytes (shaderDumpModule request))
      interface = encodeUtf8 (renderInterfaceTable request)
      stem = dumpStem request spirV interface
      spirVPath = directory </> stem <> ".spv"
      interfacePath = directory </> stem <> ".interface.txt"
      disassemblyPath = directory </> stem <> ".spvasm"
  atomicWrite directory spirVPath spirV
  atomicWrite directory interfacePath interface
  disassembler <- findDisassembler
  case disassembler of
    Nothing -> pure (Just spirVPath)
    Just executable -> Just . fromMaybe spirVPath <$> createDisassembly directory executable spirVPath disassemblyPath

{- | Retain a shader artifact for an exception even when normal diagnostic
dumping is disabled. A configured dump directory is preferred; an unusable
configuration falls back to a dedicated directory below the system temporary
directory.
-}
retainShaderFailureArtifact :: ShaderDump -> IO FilePath
retainShaderFailureArtifact =
  retainShaderFailureArtifactWith
    (lookupEnv "VPIPE_DUMP")
    getTemporaryDirectory
    disassembleWithInstalledTool

{- | Injectable failure-artifact seam. The disassembly action receives the
written SPIR-V path and intended disassembly path, and reports whether it
created the latter.
-}
retainShaderFailureArtifactWith :: IO (Maybe FilePath) -> IO FilePath -> (FilePath -> FilePath -> IO Bool) -> ShaderDump -> IO FilePath
retainShaderFailureArtifactWith lookupDirectory temporaryDirectory disassemble request = do
  configured <- lookupDirectory
  fallbackRoot <- temporaryDirectory
  let fallback = fallbackRoot </> "vpipe-shader-failures"
      preferred = case configured of
        Just directory | not (null directory) -> directory
        _ -> fallback
  first <- trySynchronous (withMVar dumpLock (\_ -> retainInDirectory preferred disassemble request))
  case first of
    Right path -> pure path
    Left error'
      | preferred == fallback -> throwIO error'
      | otherwise -> withMVar dumpLock (\_ -> retainInDirectory fallback disassemble request)

throwShaderCompileBugWith :: IO FilePath -> String -> IO a
throwShaderCompileBugWith retainArtifact detail = do
  artifact <- retainArtifact
  throwIO (ShaderCompileBug detail artifact)

throwShaderDriverFailureWith :: IO FilePath -> String -> String -> Result.Result -> IO a
throwShaderDriverFailureWith retainArtifact operation detail result =
  case classifyShaderFailure result of
    GeneratedShaderRejected -> throwShaderCompileBugWith retainArtifact detail
    ShaderDeviceLost -> throwIO DeviceLost
    OtherShaderFailure -> throwIO (VulkanFailure operation detail)

retainInDirectory :: FilePath -> (FilePath -> FilePath -> IO Bool) -> ShaderDump -> IO FilePath
retainInDirectory directory disassemble request = do
  createDirectoryIfMissing True directory
  let spirV = LazyByteString.toStrict (moduleBytes (shaderDumpModule request))
      interface = encodeUtf8 (renderInterfaceTable request)
      stem = dumpStem request spirV interface
      spirVPath = directory </> stem <> ".spv"
      interfacePath = directory </> stem <> ".interface.txt"
      disassemblyPath = directory </> stem <> ".spvasm"
  atomicWrite directory spirVPath spirV
  bestEffortAction (atomicWrite directory interfacePath interface)
  disassemblerSucceeded <- bestEffortValue False (disassemble spirVPath disassemblyPath)
  disassemblyExists <- if disassemblerSucceeded then doesFileExist disassemblyPath else pure False
  pure (if disassemblyExists then disassemblyPath else spirVPath)

disassembleWithInstalledTool :: FilePath -> FilePath -> IO Bool
disassembleWithInstalledTool spirVPath disassemblyPath = do
  executable <- findExecutable "spirv-dis"
  case executable of
    Nothing -> pure False
    Just path -> isJust <$> createDisassembly (takeDirectory disassemblyPath) path spirVPath disassemblyPath

renderInterfaceTable :: ShaderDump -> String
renderInterfaceTable request =
  unlines
    [ "vpipe shader interface"
    , "name: " <> show (shaderDumpName request)
    , "stage: " <> stageName (shaderDumpStage request)
    ]
    <> shaderDumpInterface request

dumpStem :: ShaderDump -> ByteString -> ByteString -> FilePath
dumpStem request spirV interface =
  sanitizedName
    <> "."
    <> stageName (shaderDumpStage request)
    <> "."
    <> fingerprint payload
 where
  sanitizedName = sanitizeName (shaderDumpName request)
  payload =
    encodeUtf8 (shaderDumpName request)
      <> encodeUtf8 (stageName (shaderDumpStage request))
      <> spirV
      <> interface

sanitizeName :: String -> String
sanitizeName original = case take 64 normalized of
  [] -> "module"
  value -> value
 where
  normalized =
    dropWhileEnd
      (== '-')
      (dropWhile (== '-') (collapseDashes (fmap sanitizeCharacter original)))
  sanitizeCharacter character
    | isAsciiLower character || isAsciiUpper character || isDigit character = toLower character
    | character == '-' || character == '_' || character == '.' = character
    | otherwise = '-'

collapseDashes :: String -> String
collapseDashes [] = []
collapseDashes ('-' : '-' : rest) = collapseDashes ('-' : rest)
collapseDashes (character : rest) = character : collapseDashes rest

stageName :: ShaderDumpStage -> String
stageName stage = case stage of
  DumpCompute -> "compute"
  DumpVertex -> "vertex"
  DumpFragment -> "fragment"

fingerprint :: ByteString -> String
fingerprint bytes = replicate (16 - length rendered) '0' <> rendered
 where
  rendered = showHex (ByteString.foldl' step fnvOffset bytes) ""
  step hash byte = (hash `xor` fromIntegral byte) * fnvPrime
  fnvOffset, fnvPrime :: Word64
  fnvOffset = 14695981039346656037
  fnvPrime = 1099511628211

encodeUtf8 :: String -> ByteString
encodeUtf8 = TextEncoding.encodeUtf8 . Text.pack

atomicWrite :: FilePath -> FilePath -> ByteString -> IO ()
atomicWrite directory destination bytes = mask $ \restore -> do
  (temporary, handle) <- openBinaryTempFile directory (takeFileName destination <> ".tmp")
  let cleanup = closeQuietly handle >> removeQuietly temporary
  restore (ByteString.hPut handle bytes >> hFlush handle) `onException` cleanup
  hClose handle `onException` cleanup
  renameFile temporary destination `onException` cleanup

createDisassembly :: FilePath -> FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
createDisassembly directory executable spirVPath destination = mask $ \restore -> do
  (temporary, handle) <- openBinaryTempFile directory (takeFileName destination <> ".tmp")
  hClose handle
  let cleanup = removeQuietly temporary
  (exitCode, _, _) <-
    restore (readProcessWithExitCode executable [spirVPath, "-o", temporary] "")
      `onException` cleanup
  case exitCode of
    ExitSuccess -> do
      renameFile temporary destination `onException` cleanup
      pure (Just destination)
    ExitFailure _ -> cleanup >> pure Nothing

closeQuietly :: Handle -> IO ()
closeQuietly handle = void (try (hClose handle) :: IO (Either IOException ()))

removeQuietly :: FilePath -> IO ()
removeQuietly path = void (try (removeFile path) :: IO (Either IOException ()))

bestEffort :: IO (Maybe FilePath) -> IO (Maybe FilePath)
bestEffort action =
  action `catch` \(error' :: SomeException) ->
    case fromException error' of
      Just asynchronous -> throwIO (asynchronous :: AsyncException)
      Nothing -> pure Nothing

bestEffortAction :: IO () -> IO ()
bestEffortAction action = void (bestEffortValue () action)

bestEffortValue :: a -> IO a -> IO a
bestEffortValue fallback action = do
  result <- trySynchronous action
  pure (fromRight fallback result)

trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous action =
  (Right <$> action) `catch` \(error' :: SomeException) ->
    case fromException error' of
      Just asynchronous -> throwIO (asynchronous :: AsyncException)
      Nothing -> pure (Left error')

{-# NOINLINE dumpLock #-}
dumpLock :: MVar ()
dumpLock = unsafePerformIO (newMVar ())
