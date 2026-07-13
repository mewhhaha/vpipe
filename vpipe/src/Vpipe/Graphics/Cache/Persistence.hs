{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}

module Vpipe.Graphics.Cache.Persistence (
  pipelineCachePath,
  pipelineCacheFile,
  readPipelineCacheFile,
  writePipelineCacheFile,
) where

import Control.Exception (AsyncException, SomeException, catch, fromException, onException, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word8)
import Numeric (showHex)
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, getXdgDirectory, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, hFlush, openBinaryTempFile)

pipelineCachePath :: FilePath -> ByteString -> FilePath
pipelineCachePath cacheDirectory uuid =
  cacheDirectory </> "vpipe" </> "pipeline-cache" </> (uuidHex uuid <> ".bin")

pipelineCacheFile :: ByteString -> IO FilePath
pipelineCacheFile uuid = do
  cacheDirectory <- getXdgDirectory XdgCache ""
  pure (pipelineCachePath cacheDirectory uuid)

readPipelineCacheFile :: FilePath -> IO ByteString
readPipelineCacheFile path = ignoreSynchronous (ByteString.readFile path) ByteString.empty

writePipelineCacheFile :: FilePath -> ByteString -> IO ()
writePipelineCacheFile path bytes = do
  let directory = takeDirectory path
  createDirectoryIfMissing True directory
  (temporaryPath, handle) <- openBinaryTempFile directory (takeFileName path <> ".tmp")
  let cleanup = ignoreSynchronous (hClose handle) () >> ignoreSynchronous (removeFile temporaryPath) ()
  (ByteString.hPut handle bytes >> hFlush handle >> hClose handle >> renameFile temporaryPath path) `onException` cleanup

uuidHex :: ByteString -> String
uuidHex = concatMap byteHex . ByteString.unpack

byteHex :: Word8 -> String
byteHex byte = case showHex byte "" of
  [digit] -> ['0', digit]
  digits -> digits

ignoreSynchronous :: IO a -> a -> IO a
ignoreSynchronous action fallback =
  action `catch` \(error' :: SomeException) ->
    case fromException error' of
      Just asynchronous -> throwIO (asynchronous :: AsyncException)
      Nothing -> pure fallback
