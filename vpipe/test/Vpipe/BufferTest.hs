{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Vpipe.BufferTest (bufferTests) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, tryReadMVar)
import Control.Exception (IOException, SomeException, displayException, throwIO, try)
import Control.Monad (forM, forM_, replicateM, replicateM_, void)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Linear (V2 (..), V3 (..), V4 (..))
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Vulkan.Core13.Enums.AccessFlags2 qualified as Access2
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2

import Vpipe.Buffer (Buffer, RestartIndex, Usage (..), bufferLayout, bufferLength, bufferStride, destroyBuffer, newBuffer, normalIndex, primitiveRestartIndex, readBuffer, restartIndexWord32, writeBuffer, writeIndexBuffer)
import Vpipe.Buffer.Dynamic (DynamicBuffer, destroyDynamicBuffer, dynamicSliceBytes, dynamicSliceOffset, newDynamicBuffer, readDynamicBuffer, writeDynamicBuffer)
import Vpipe.Buffer.Dynamic.Internal (checkDynamicDescriptorOffset, commitDynamicHostWrite)
import Vpipe.Buffer.Format qualified as Format
import Vpipe.Buffer.Staging (RingReservation (..), planRingReservation, retirementActionForTest)
import Vpipe.Buffer.State qualified as State
import Vpipe.Context (Context, VpipeConfig (vpipeValidationStrict), contextNonCoherentAtomSize, contextStorageBufferOffsetAlignment, contextUniformBufferOffsetAlignment, defaultVpipeConfig, transferQueue, withVpipe)
import Vpipe.Context.Internal (contextAllocationCountForTest)
import Vpipe.Context.Queue (Queue)
import Vpipe.Context.Queue.Internal (currentTimelineValueForTest)
import Vpipe.Error (VpipeError (BufferElementRangeInvalid, BufferReleased, ContextClosed, DynamicBufferDescriptorOffsetMisaligned, NoVulkanIcd, ResourceQuarantined))

bufferTests :: TestTree
bufferTests =
  testGroup
    "buffer"
    [ testCase "writes and reads a partial Word32 range" roundTripTest
    , testCase "rejects element ranges outside the buffer" boundsTest
    , testCase "retires and wraps persistent staging ranges" ringWrapTest
    , testCase "pads staging reservations to non-coherent atoms" stagingAtomPlannerTest
    , testCase "wraps atom-padded staging reservations without overlap" stagingWrapPlannerTest
    , testCase "keeps staging retirement one-shot after a later transaction failure" stagingRetirementRollbackTest
    , testCase "waits for staging retirement cleanup before freeing the command buffer" stagingRetirementGateTest
    , testCase "uploads values larger than the persistent ring" oversizedUploadTest
    , testCase "uploads separate buffers concurrently" concurrentUploadTest
    , testCase "creates and destroys buffers concurrently" concurrentResourceCreationTest
    , testCase "blocks overlapping state reservations and rejects stale commits" stateReservationTest
    , testCase "quarantined buffer state wakes waiters and rejects later reads" stateQuarantineTest
    , testCase "chooses layout from typed usage" layoutSelectionTest
    , testCase "keeps dynamic mapped copies isolated" dynamicCopyIsolationTest
    , testCase "failed dynamic host writes preserve the previous use and release the reservation" dynamicHostWriteFailureTest
    , testCase "aligns dynamic slices for descriptor offsets" dynamicDescriptorAlignmentTest
    , testCase "accepts aligned and rejects misaligned dynamic descriptor offsets" dynamicDescriptorSubrangeAlignmentTest
    , testCase "releases normal and dynamic buffers before context shutdown" explicitReleaseTest
    , testCase "cancelling submitted readback retains its allocation through retirement" cancelledReadbackTest
    , testCase "retained buffers reject every operation after context close" retainedBufferClosureTest
    , testCase "reserves the primitive-restart index" restartIndexTest
    , testCase "round trips every BufferFormat constructor" bufferFormatConstructorRoundTripsTest
    , testCase "creates and uploads one thousand tiny buffers" tinyBufferStressTest
    ]

withTestContext :: (Context -> IO a) -> IO a
withTestContext action = do
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  let config = defaultVpipeConfig{vpipeValidationStrict = requested == Just "lavapipe"}
  result <- try (withVpipe config action)
  case result of
    Left (NoVulkanIcd detail) | requested /= Just "lavapipe" -> error ("SKIP: Vulkan ICD unavailable: " <> detail)
    Left error' -> throwIO (error' :: VpipeError)
    Right value -> pure value

roundTripTest :: IO ()
roundTripTest = withTestContext $ \context -> do
  buffer <- newBuffer context 4 :: IO (Buffer '[CopySrc, CopyDst] Word32)
  writeBuffer buffer 0 [1, 2, 3, 4]
  writeBuffer buffer 1 [20, 30]
  bufferLength buffer @?= 4
  values <- readBuffer buffer 0 4
  values @?= [1, 20, 30, 4]

boundsTest :: IO ()
boundsTest = withTestContext $ \context -> do
  buffer <- newBuffer context 2 :: IO (Buffer '[CopySrc] Word32)
  result <- try (writeBuffer buffer 2 [1]) :: IO (Either VpipeError ())
  case result of
    Left BufferElementRangeInvalid{} -> pure ()
    unexpected -> assertBool ("expected bounds error, got " <> show unexpected) False

ringWrapTest :: IO ()
ringWrapTest = withTestContext $ \context -> do
  let elementCount = 65536
  buffers <- replicateM 6 (newBuffer context elementCount :: IO (Buffer '[CopySrc, CopyDst] Word32))
  forM_ (zip [1 ..] buffers) $ \(value, buffer) ->
    writeBuffer buffer 0 (replicate elementCount value)
  actual <- traverse (\buffer -> readBuffer buffer 0 1) buffers
  actual @?= fmap (pure . fromIntegral) ([1 .. 6] :: [Int])

stagingAtomPlannerTest :: IO ()
stagingAtomPlannerTest = do
  planRingReservation 128 32 4 1 0 Nothing @?= Just (RingReservation 0 32)
  planRingReservation 128 32 4 1 32 (Just 0) @?= Just (RingReservation 32 64)
  planRingReservation 256 64 12 1 64 (Just 0) @?= Just (RingReservation 192 256)

stagingWrapPlannerTest :: IO ()
stagingWrapPlannerTest = do
  planRingReservation 128 32 8 33 96 (Just 64) @?= Just (RingReservation 0 64)
  planRingReservation 32 64 4 1 0 Nothing @?= Nothing

stagingRetirementRollbackTest :: IO ()
stagingRetirementRollbackTest = do
  cleanupCount <- newMVar (0 :: Int)
  commandBufferFreeCount <- newMVar (0 :: Int)
  retirement <-
    retirementActionForTest
      (modifyMVar_ cleanupCount (pure . (+ 1)))
      (modifyMVar_ commandBufferFreeCount (pure . (+ 1)))
  failed <- try (retirement >> throwIO (userError "injected transaction failure")) :: IO (Either IOException ())
  case failed of
    Left _ -> pure ()
    Right () -> assertFailure "injected transaction failure unexpectedly succeeded"
  retirement
  readMVar cleanupCount >>= (@?= 1)
  readMVar commandBufferFreeCount >>= (@?= 1)

stagingRetirementGateTest :: IO ()
stagingRetirementGateTest = do
  retirementGate <- newEmptyMVar
  cleanupStarted <- newEmptyMVar
  retirementOrder <- newMVar ([] :: [String])
  retirement <-
    retirementActionForTest
      (putMVar cleanupStarted () >> takeMVar retirementGate >> appendRetirementStep retirementOrder "cleanup")
      (appendRetirementStep retirementOrder "free command buffer")
  completion <- forkResult retirement
  timeout 1000000 (takeMVar cleanupStarted) >>= maybe (assertFailure "staging retirement did not reach the cleanup gate") pure
  tryReadMVar completion >>= maybe (pure ()) (const (assertFailure "command buffer was freed before the retirement gate opened"))
  readMVar retirementOrder >>= (@?= [])
  putMVar retirementGate ()
  result <- timeout 1000000 (takeMVar completion)
  maybe (assertFailure "staging retirement did not complete after the cleanup gate opened") (either throwIO pure) result
  readMVar retirementOrder >>= (@?= ["cleanup", "free command buffer"])

appendRetirementStep :: MVar [String] -> String -> IO ()
appendRetirementStep order step = modifyMVar_ order (pure . (<> [step]))

oversizedUploadTest :: IO ()
oversizedUploadTest = withTestContext $ \context -> do
  let elementCount = 300000
      values = [0 .. fromIntegral elementCount - 1]
  buffer <- newBuffer context elementCount :: IO (Buffer '[CopySrc, CopyDst] Word32)
  writeBuffer buffer 0 values
  first <- readBuffer buffer 0 1
  last' <- readBuffer buffer (elementCount - 1) 1
  first @?= [0]
  last' @?= [fromIntegral elementCount - 1]

concurrentUploadTest :: IO ()
concurrentUploadTest = withTestContext $ \context -> do
  left <- newBuffer context 4096 :: IO (Buffer '[CopySrc, CopyDst] Word32)
  right <- newBuffer context 4096 :: IO (Buffer '[CopySrc, CopyDst] Word32)
  completions <-
    traverse
      forkResult
      [ writeBuffer left 0 (replicate 4096 17)
      , writeBuffer right 0 (replicate 4096 29)
      ]
  results <- traverse takeMVar completions
  forM_ results (either throwIO pure)
  leftValue <- readBuffer left 0 1
  rightValue <- readBuffer right 0 1
  leftValue @?= [17]
  rightValue @?= [29]

concurrentResourceCreationTest :: IO ()
concurrentResourceCreationTest = withTestContext $ \context -> do
  completions <-
    forM [1 .. 8 :: Word32] $ \value ->
      forkResult $
        replicateM_ 32 $ do
          buffer <- newBuffer context 1 :: IO (Buffer '[CopySrc, CopyDst] Word32)
          writeBuffer buffer 0 [value]
          readBuffer buffer 0 1 >>= (@?= [value])
          destroyBuffer buffer
  results <- traverse takeMVar completions
  forM_ results (either throwIO pure)

stateReservationTest :: IO ()
stateReservationTest = do
  state <- State.newBufferState
  first <- State.beginBufferUse state
  secondResult <- newEmptyMVar
  _ <- forkIO (State.beginBufferUse state >>= putMVar secondResult)
  threadDelay 10000
  tryReadMVar secondResult >>= maybe (pure ()) (const (assertFailure "overlapping reservation did not block"))
  let queue = error "pure buffer state test forced a Vulkan Queue"
      use =
        State.BufferUse
          { State.bufferUseStage = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
          , State.bufferUseAccess = Access2.ACCESS_2_TRANSFER_WRITE_BIT
          , State.bufferUseCompletion =
              Just
                State.BufferCompletion
                  { State.bufferCompletionQueue = queue
                  , State.bufferCompletionQueueFamily = 1
                  , State.bufferCompletionTimeline = 1
                  }
          }
  firstCommitted <- State.commitBufferUse first use
  second <- takeMVar secondResult
  State.reservationPreviousUse second @?= Just use
  staleCommit <- State.commitBufferUse first use
  currentCommit <- State.commitBufferUse second use
  committed <- State.lastBufferUse state
  firstCommitted @?= True
  staleCommit @?= False
  currentCommit @?= True
  committed @?= Just use

stateQuarantineTest :: IO ()
stateQuarantineTest = do
  state <- State.newBufferState
  active <- State.beginBufferUse state
  started <- newEmptyMVar
  waitingResult <- newEmptyMVar :: IO (MVar (Either VpipeError State.Reservation))
  _ <- forkIO $ do
    putMVar started ()
    try (State.beginBufferUse state) >>= putMVar waitingResult
  takeMVar started
  State.quarantineBufferState state
  timeout 100_000 (takeMVar waitingResult)
    >>= maybe (assertFailure "quarantine did not wake the waiting buffer reservation") assertQuarantined
  lastUse <- try (State.lastBufferUse state)
  assertQuarantined (lastUse :: Either VpipeError (Maybe State.BufferUse))
  State.cancelBufferUse active >>= (@?= False)
 where
  assertQuarantined result = case result of
    Left ResourceQuarantined -> pure ()
    Left error' -> assertFailure ("expected ResourceQuarantined, got " <> show error')
    Right _ -> assertFailure "quarantined buffer state operation succeeded"

layoutSelectionTest :: IO ()
layoutSelectionTest = withTestContext $ \context -> do
  vertex <- newBuffer context 1 :: IO (Buffer '[Vertex] Word32)
  uniform <- newBuffer context 1 :: IO (Buffer '[Uniform] Word32)
  storage <- newBuffer context 1 :: IO (Buffer '[Storage] Word32)
  bufferLayout vertex @?= Format.Vertex
  bufferLayout uniform @?= Format.Std140
  bufferLayout storage @?= Format.Std430
  bufferStride vertex @?= 4
  bufferStride uniform @?= 4
  bufferStride storage @?= 4

dynamicCopyIsolationTest :: IO ()
dynamicCopyIsolationTest = withTestContext $ \context -> do
  buffer <- newDynamicBuffer context 2 3 :: IO (DynamicBuffer '[Storage] Word32)
  writeDynamicBuffer buffer 0 0 [10, 11, 12]
  writeDynamicBuffer buffer 1 0 [20, 21, 22]
  readDynamicBuffer buffer 0 0 3 >>= (@?= [10, 11, 12])
  readDynamicBuffer buffer 1 0 3 >>= (@?= [20, 21, 22])
  dynamicSliceOffset buffer 0 @?= Just 0
  assertBool "copies use distinct mapped slices" (dynamicSliceOffset buffer 1 /= Just 0)

dynamicHostWriteFailureTest :: IO ()
dynamicHostWriteFailureTest = do
  state <- State.newBufferState
  previousReservation <- State.beginBufferUse state
  let previousUse =
        State.BufferUse
          { State.bufferUseStage = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
          , State.bufferUseAccess = Access2.ACCESS_2_TRANSFER_WRITE_BIT
          , State.bufferUseCompletion = Nothing
          }
  State.commitBufferUse previousReservation previousUse >>= (@?= True)

  failed <- try (commitDynamicHostWrite state (throwIO (userError "injected dynamic host write failure"))) :: IO (Either IOException ())
  case failed of
    Left _ -> pure ()
    Right () -> assertFailure "injected dynamic host write unexpectedly succeeded"
  State.lastBufferUse state >>= (@?= Just previousUse)

  available <- timeout 1000000 (State.beginBufferUse state)
  reservation <- maybe (assertFailure "failed dynamic host write left its reservation active") pure available
  State.cancelBufferUse reservation >>= (@?= True)

  commitDynamicHostWrite state (pure ())
  committed <- State.lastBufferUse state
  fmap State.bufferUseStage committed @?= Just Stage2.PIPELINE_STAGE_2_HOST_BIT
  fmap State.bufferUseAccess committed @?= Just Access2.ACCESS_2_HOST_WRITE_BIT

dynamicDescriptorAlignmentTest :: IO ()
dynamicDescriptorAlignmentTest = withTestContext $ \context -> do
  uniform <- newDynamicBuffer context 2 1 :: IO (DynamicBuffer '[Uniform] Word32)
  storage <- newDynamicBuffer context 2 1 :: IO (DynamicBuffer '[Storage] Word32)
  let uniformAlignment = fromIntegral (contextUniformBufferOffsetAlignment context)
      storageAlignment = fromIntegral (contextStorageBufferOffsetAlignment context)
      atomAlignment = fromIntegral (contextNonCoherentAtomSize context)
  assertBool "uniform slices satisfy minUniformBufferOffsetAlignment" (dynamicSliceBytes uniform `mod` max 1 uniformAlignment == 0)
  assertBool "storage slices satisfy minStorageBufferOffsetAlignment" (dynamicSliceBytes storage `mod` max 1 storageAlignment == 0)
  assertBool "uniform slices occupy whole non-coherent atoms" (dynamicSliceBytes uniform `mod` max 1 atomAlignment == 0)
  assertBool "storage slices occupy whole non-coherent atoms" (dynamicSliceBytes storage `mod` max 1 atomAlignment == 0)

dynamicDescriptorSubrangeAlignmentTest :: IO ()
dynamicDescriptorSubrangeAlignmentTest = do
  checkDynamicDescriptorOffset 16 4 16 @?= Right ()
  let misaligned = DynamicBufferDescriptorOffsetMisaligned 1 4 16
  checkDynamicDescriptorOffset 16 1 4 @?= Left misaligned
  displayException misaligned
    @?= "Dynamic buffer element offset 1 begins at byte offset 4, which is not aligned to the required descriptor offset alignment of 16 bytes. Choose an element offset whose byte offset is a multiple of 16."

explicitReleaseTest :: IO ()
explicitReleaseTest = withTestContext $ \context -> do
  baseline <- contextAllocationCountForTest context
  buffer <- newBuffer context 1 :: IO (Buffer '[CopySrc, CopyDst] Word32)
  writeBuffer buffer 0 [7]
  destroyBuffer buffer
  destroyBuffer buffer
  releasedBuffer <- try (writeBuffer buffer 0 [9])
  releasedBuffer @?= Left BufferReleased
  dynamic <- newDynamicBuffer context 2 1 :: IO (DynamicBuffer '[Uniform] Word32)
  destroyDynamicBuffer dynamic
  destroyDynamicBuffer dynamic
  releasedDynamic <- try (writeDynamicBuffer dynamic 0 0 [9])
  releasedDynamic @?= Left BufferReleased
  replicateM_ 128 $ do
    temporary <- newBuffer context 1 :: IO (Buffer '[CopySrc, CopyDst] Word32)
    destroyBuffer temporary
    temporaryDynamic <- newDynamicBuffer context 2 1 :: IO (DynamicBuffer '[Uniform] Word32)
    destroyDynamicBuffer temporaryDynamic
  contextAllocationCountForTest context >>= (@?= baseline)

cancelledReadbackTest :: IO ()
cancelledReadbackTest = withTestContext $ \context -> do
  let elementCount = 2 * 1024 * 1024
      queue = transferQueue context
  buffer <- newBuffer context elementCount :: IO (Buffer '[CopySrc, CopyDst] Word32)
  writeBuffer buffer 0 (replicate elementCount 17)
  before <- currentTimelineValueForTest queue
  completion <- newEmptyMVar
  worker <- forkIO (try (readBuffer buffer 0 elementCount) >>= putMVar completion)
  waitForSubmission queue before completion 100000
  killThread worker
  result <- takeMVar completion
  case result of
    Left (_ :: SomeException) -> pure ()
    Right _ -> assertFailure "readback completed before cancellation could exercise retirement"
  marker <- newBuffer context 1 :: IO (Buffer '[CopySrc, CopyDst] Word32)
  writeBuffer marker 0 [23]
  readBuffer marker 0 1 >>= (@?= [23])

waitForSubmission :: Queue -> Word64 -> MVar (Either SomeException [Word32]) -> Int -> IO ()
waitForSubmission queue previous completion remaining
  | remaining == 0 = assertFailure "readback did not submit before the cancellation timeout"
  | otherwise = do
      finished <- tryReadMVar completion
      case finished of
        Just _ -> assertFailure "readback completed before its submission could be observed"
        Nothing -> do
          current <- currentTimelineValueForTest queue
          if current > previous
            then pure ()
            else threadDelay 10 >> waitForSubmission queue previous completion (remaining - 1)

retainedBufferClosureTest :: IO ()
retainedBufferClosureTest = do
  (buffer, dynamic) <- withTestContext $ \context -> do
    retainedBuffer <- newBuffer context 1 :: IO (Buffer '[CopySrc, CopyDst] Word32)
    retainedDynamic <- newDynamicBuffer context 2 1 :: IO (DynamicBuffer '[Uniform] Word32)
    pure (retainedBuffer, retainedDynamic)
  forM_
    [ writeBuffer buffer 0 [1]
    , void (readBuffer buffer 0 1)
    , destroyBuffer buffer
    , writeDynamicBuffer dynamic 0 0 [1]
    , void (readDynamicBuffer dynamic 0 0 1)
    , destroyDynamicBuffer dynamic
    ]
    assertContextClosed

assertContextClosed :: IO () -> IO ()
assertContextClosed action = do
  result <- try action
  result @?= Left ContextClosed

restartIndexTest :: IO ()
restartIndexTest = withTestContext $ \context -> do
  buffer <- newBuffer context 3 :: IO (Buffer '[Index, CopySrc] Word32)
  let first = fromMaybe (error "normal index rejected") (normalIndex 7)
  normalIndex maxBound @?= Nothing
  writeIndexBuffer buffer 0 [first, primitiveRestartIndex, first]
  values <- readBuffer buffer 0 3
  values @?= [7, maxBound, 7]
  restartIndexWord32 (primitiveRestartIndex :: RestartIndex) @?= maxBound

data RoundTripRecord = RoundTripRecord Float (V3 Float)
  deriving (Eq, Show, Generic)

bufferFormatConstructorRoundTripsTest :: IO ()
bufferFormatConstructorRoundTripsTest = withTestContext $ \context -> do
  roundTripFormat @Float context [1.25, -2.5]
  roundTripFormat @Int32 context [7, -11]
  roundTripFormat @Word32 context [3, 9]
  roundTripFormat @Bool context [True, False]
  roundTripFormat @(V2 Float) context [V2 1 2, V2 (-3) 4]
  roundTripFormat @(V3 Float) context [V3 1 2 3, V3 (-4) 5 (-6)]
  roundTripFormat @(V4 Float) context [V4 1 2 3 4, V4 (-5) 6 (-7) 8]
  roundTripFormat @(V2 (V3 Float)) context [V2 (V3 1 2 3) (V3 4 5 6)]
  roundTripFormat @(V3 (V2 Word32)) context [V3 (V2 1 2) (V2 3 4) (V2 5 6)]
  roundTripFormat @(V4 Bool) context [V4 True False True False]
  roundTripFormat @(Format.MatrixBuffer 2 3 Float) context [Format.MatrixBuffer (V2 (V3 1 2 3) (V3 4 5 6))]
  roundTripFormat @(Format.MatrixBuffer 3 2 Float) context [Format.MatrixBuffer (V3 (V2 1 2) (V2 3 4) (V2 5 6))]
  roundTripFormat @(Format.MatrixBuffer 4 4 Float) context [Format.MatrixBuffer (V4 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) (V4 13 14 15 16))]
  roundTripFormat @(V3 Float, Int32) context [(V3 1 2 3, -7)]
  let record = RoundTripRecord 3.5 (V3 4.5 5.5 6.5)
  roundTripFormat @(Format.Generically RoundTripRecord) context [record]
  roundTripFormat @(Format.GenericHost RoundTripRecord RoundTripRecord) context [record]

roundTripFormat :: forall (a :: Type). (Format.BufferFormat a, Eq (Format.HostFormat a), Show (Format.HostFormat a)) => Context -> [Format.HostFormat a] -> IO ()
roundTripFormat context expected = do
  buffer <- newBuffer context (length expected) :: IO (Buffer '[CopySrc, CopyDst] a)
  writeBuffer buffer 0 expected
  actual <- readBuffer buffer 0 (length expected)
  actual @?= expected

tinyBufferStressTest :: IO ()
tinyBufferStressTest = withTestContext $ \context -> do
  buffers <-
    forM [0 .. 999 :: Word32] $ \value -> do
      buffer <- newBuffer context 1 :: IO (Buffer '[CopySrc, CopyDst] Word32)
      writeBuffer buffer 0 [value]
      pure buffer
  case (buffers, drop 500 buffers, reverse buffers) of
    (firstBuffer : _, middleBuffer : _, lastBuffer : _) -> do
      first <- readBuffer firstBuffer 0 1
      middle <- readBuffer middleBuffer 0 1
      last' <- readBuffer lastBuffer 0 1
      first @?= [0]
      middle @?= [500]
      last' @?= [999]
    _ -> assertBool "expected one thousand buffers" False

forkResult :: IO a -> IO (MVar (Either SomeException a))
forkResult action = do
  completion <- newEmptyMVar
  _ <- forkIO (try action >>= putMVar completion)
  pure completion
