module Vpipe.ImageStateTest (imageStateTests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryReadMVar)
import Control.Exception (try)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Vulkan.Core10.Enums.ImageLayout qualified as Layout
import Vulkan.Core13.Enums.AccessFlags2 qualified as Access2
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2

import Vpipe.Error (VpipeError (ResourceQuarantined))
import Vpipe.Image.State

imageStateTests :: TestTree
imageStateTests =
  testGroup
    "image state"
    [ testCase "tracks independent mip and layer subresources" independentSubresourcesTest
    , testCase "blocks overlapping reservations and rejects stale commits" staleReservationTest
    , testCase "quarantined image state wakes waiters and rejects later reads" quarantineTest
    , testCase "rejects subresources outside mip and layer bounds" boundsTest
    , testCase "starts every subresource in the undefined layout" initialLayoutTest
    ]

transferWrite :: ImageUse
transferWrite =
  ImageUse
    { imageUseLayout = Layout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    , imageUseStage = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
    , imageUseAccess = Access2.ACCESS_2_TRANSFER_WRITE_BIT
    , imageUseCompletion = Nothing
    }

independentSubresourcesTest :: IO ()
independentSubresourcesTest = do
  state <- newImageState 2 2
  first <- beginImageUse state [ImageSubresource 0 0]
  second <- beginImageUse state [ImageSubresource 1 1]
  firstCommitted <- commitImageUse first transferWrite
  secondCommitted <- commitImageUse second transferWrite
  firstUse <- lastImageUse state (ImageSubresource 0 0)
  secondUse <- lastImageUse state (ImageSubresource 1 1)
  untouchedUse <- lastImageUse state (ImageSubresource 0 1)
  firstCommitted @?= True
  secondCommitted @?= True
  firstUse @?= Just transferWrite
  secondUse @?= Just transferWrite
  untouchedUse @?= Nothing

staleReservationTest :: IO ()
staleReservationTest = do
  state <- newImageState 1 1
  first <- beginImageUse state [ImageSubresource 0 0]
  secondResult <- newEmptyMVar
  _ <- forkIO (beginImageUse state [ImageSubresource 0 0] >>= putMVar secondResult)
  threadDelay 10000
  tryReadMVar secondResult >>= maybe (pure ()) (const (assertBool "overlapping reservation did not block" False))
  firstCommitted <- commitImageUse first transferWrite
  second <- takeMVar secondResult
  reservationPreviousUses second @?= [(ImageSubresource 0 0, Just transferWrite)]
  staleCommitted <- commitImageUse first transferWrite
  currentCommitted <- commitImageUse second transferWrite
  firstCommitted @?= True
  staleCommitted @?= False
  currentCommitted @?= True

boundsTest :: IO ()
boundsTest = do
  state <- newImageState 2 3
  mipResult <- try (beginImageUse state [ImageSubresource 2 0]) :: IO (Either ImageSubresourceOutOfBounds ImageReservation)
  layerResult <- try (lastImageUse state (ImageSubresource 0 3)) :: IO (Either ImageSubresourceOutOfBounds (Maybe ImageUse))
  assertOutOfBounds mipResult
  assertOutOfBounds layerResult
 where
  assertOutOfBounds (Left _) = pure ()
  assertOutOfBounds (Right _) = assertBool "expected ImageSubresourceOutOfBounds" False

initialLayoutTest :: IO ()
initialLayoutTest = do
  state <- newImageState 2 2
  layout <- imageSubresourceLayout state (ImageSubresource 1 1)
  layout @?= Layout.IMAGE_LAYOUT_UNDEFINED

quarantineTest :: IO ()
quarantineTest = do
  state <- newImageState 1 1
  active <- beginImageUse state [ImageSubresource 0 0]
  started <- newEmptyMVar
  waitingResult <- newEmptyMVar :: IO (MVar (Either VpipeError ImageReservation))
  _ <- forkIO $ do
    putMVar started ()
    try (beginImageUse state [ImageSubresource 0 0]) >>= putMVar waitingResult
  takeMVar started
  quarantineImageState state
  timeout 100_000 (takeMVar waitingResult)
    >>= maybe (assertFailure "quarantine did not wake the waiting image reservation") assertQuarantined
  lastUse <- try (lastImageUse state (ImageSubresource 0 0))
  assertQuarantined (lastUse :: Either VpipeError (Maybe ImageUse))
  uses <- try (allImageUses state)
  assertQuarantined (uses :: Either VpipeError [ImageUse])
  layout <- try (imageSubresourceLayout state (ImageSubresource 0 0))
  assertQuarantined (layout :: Either VpipeError Layout.ImageLayout)
  cancelImageUse active >>= (@?= False)
 where
  assertQuarantined result = case result of
    Left ResourceQuarantined -> pure ()
    Left error' -> assertFailure ("expected ResourceQuarantined, got " <> show error')
    Right _ -> assertFailure "quarantined image state operation succeeded"
