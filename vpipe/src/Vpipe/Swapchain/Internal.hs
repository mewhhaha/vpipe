{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}

module Vpipe.Swapchain.Internal (
  Swapchain,
  SwapchainConfig (..),
  PresentMode (..),
  PresentResult (..),
  DeferredReason (..),
  defaultSwapchainConfig,
  newSwapchain,
  destroySwapchain,
  swapchainExtent,
  -- Internal integration surface for Vpipe.Frame.
  Generation (..),
  GenerationImage (..),
  FrameSlot (..),
  FrameOwnership (..),
  SlotState,
  SlotStateView (..),
  RenderFinishedState (..),
  AcquireDriverOutcome (..),
  PresentDriverOutcome (..),
  AcquireOutcome (..),
  QueuePresentOutcome (..),
  DescriptorFrameStorage,
  CoreState (..),
  RuntimeState (..),
  FrameDomain,
  LockedSwapchain,
  withSwapchainOperation,
  withSwapchainOperationLeased,
  lockedSwapchainContext,
  lockedSwapchainFrameDomain,
  lockedSwapchainFrameConfiguration,
  lockedSwapchainExtent,
  withOperationMutexForTest,
  takeRecreationNotificationLocked,
  activeCoreState,
  poisonCoreState,
  releaseCoreState,
  claimFrameSlotLocked,
  acquireNextImageLocked,
  publishSlotAcquiredLocked,
  publishAcceptedSubmissionLocked,
  publishAcquireRecoveryLocked,
  publishPresentWaitLocked,
  presentGenerationImageLocked,
  poisonSwapchainLocked,
  inspectSlotStateLocked,
  descriptorFrameForSlotLocked,
  prepareFrameSlot,
  acquireSlotTransition,
  submitSlotTransition,
  completeSlotTransition,
  submitRenderFinishedTransition,
  queuePresentWaitTransition,
  reacquireRenderFinishedTransition,
  classifyAcquireDriverResult,
  classifyPresentDriverResult,
  presentDriverWaitAccepted,
  maximumAcquireTimeoutNanoseconds,
  finiteAcquireTimeoutNanoseconds,
  validateAcquiredImageIndex,
  runAcquireDriverCallForTest,
  runPresentDriverCallForTest,
  publishPresentDriverOutcomeForTest,
  submitPreparedFrameForTest,
  newIdleSlotStateForTest,
  retainAcquiredSlotForTest,
  newSubmittedSlotStateForTest,
  retireSlotStateForTest,
  forceDrainSlotStateForTest,
  inspectSlotStateForTest,
  newCleanupActionsForTest,
  runCleanupActionsForTest,
  pendingCleanupActionsForTest,
  lookupOrCreateSlotValueForTest,
  resetSlotValuesForTest,
  replaceGeneration,
  replaceGenerationLocked,
  runIrreversibleRecreationForTest,
  validateFramesInFlight,
  chooseSurfaceFormat,
  chooseImageCount,
  choosePresentMode,
  ExtentChoice (..),
  chooseExtent,
  FamilySharing (..),
  chooseFamilySharing,
  enumerateCompleteForTest,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, newMVar, readMVar, withMVar)
import Control.Exception (AsyncException, Exception, SomeException, catch, fromException, mask, mask_, onException, throwIO, try)
import Control.Monad (forM, unless, void, when)
import Data.Bits ((.&.), (.|.))
import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Data.Unique (Unique, newUnique)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word32, Word64)
import Foreign.Ptr (nullPtr, ptrToWordPtr)
import Vulkan.Core10.CommandBuffer qualified as CommandBuffer
import Vulkan.Core10.CommandPool qualified as CommandPool
import Vulkan.Core10.Enums.CommandBufferLevel qualified as CommandLevel
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits qualified as PoolFlags
import Vulkan.Core10.Enums.Format qualified as Format
import Vulkan.Core10.Enums.ImageAspectFlagBits qualified as Aspect
import Vulkan.Core10.Enums.ImageUsageFlagBits qualified as Usage
import Vulkan.Core10.Enums.ImageViewType qualified as ViewType
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Enums.SampleCountFlagBits qualified as Samples
import Vulkan.Core10.Enums.SharingMode qualified as Sharing
import Vulkan.Core10.FundamentalTypes qualified as Fundamental
import Vulkan.Core10.Handles qualified as Vk
import Vulkan.Core10.ImageView qualified as ImageView
import Vulkan.Core10.Queue qualified as VkQueue
import Vulkan.Core10.QueueSemaphore qualified as Semaphore
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Exception qualified as Vulkan
import Vulkan.Extensions.Handles qualified as Extensions
import Vulkan.Extensions.VK_KHR_surface qualified as Surface
import Vulkan.Extensions.VK_KHR_swapchain qualified as KHR
import Vulkan.Zero (zero)

import Vpipe.Context.Internal (Context, contextDevice, contextIdentity, contextOwnsSurface, contextPhysicalDevice, derivedObjectName, graphicsQueue, registerContextFinalizerLeased, setObjectNameLeased, withContextLease)
import Vpipe.Context.Queue.Internal (BinarySemaphoreSignal (..), BinarySemaphoreWait (..), queueFamilyIndex, submitCommandBuffersWithLeased, waitTimelineLeased, withQueueHandleLockedLeased)
import Vpipe.Descriptor.Internal (DescriptorFrame, DescriptorLayout, DescriptorLayoutIdentity, descriptorLayoutIdentity, destroyDescriptorFrameLeased, newDescriptorFrameLeased, resetDescriptorFrameLeased)
import Vpipe.Error (VpipeError (..))
import Vpipe.Format (Format (B8G8R8A8Srgb))
import Vpipe.Image.State (ImageState, newImageState, quarantineImageState)
import Vpipe.Pipeline.Internal qualified as Pipeline
import Vpipe.Pipeline.Resource.Internal (ImageBindingMetadata (..), managedImageRuntimeHandleWithQuarantine)
import Vpipe.Resource.Lifetime (LifetimeGate)
import Vpipe.Resource.Lifetime qualified as Lifetime
import Vpipe.Surface.Internal (Surface, surfaceFramebufferExtent, surfaceHandle, surfacePresentQueue)

data PresentMode = Fifo | Mailbox | Immediate
  deriving stock (Eq, Ord, Show)

data SwapchainConfig = SwapchainConfig
  { framesInFlight :: Int
  , presentModePreference :: PresentMode
  , acquireTimeoutNanoseconds :: Word64
  }
  deriving stock (Eq, Show)

defaultSwapchainConfig :: SwapchainConfig
defaultSwapchainConfig = SwapchainConfig 2 Fifo 1_000_000_000

data DeferredReason = FramebufferMinimized | AcquireTimedOut | RecreatePending
  deriving stock (Eq, Ord, Show)

data PresentResult
  = Presented
      { presentedExtent :: (Word32, Word32)
      , swapchainRecreated :: Bool
      }
  | PresentDeferred DeferredReason
  deriving stock (Eq, Show)

type DescriptorFrameStorage = [(DescriptorLayoutIdentity, DescriptorFrame)]

data FrameOwnership = FrameOwnership
  { frameOwnershipGeneration :: Word64
  , frameOwnershipImage :: Word32
  }
  deriving stock (Eq, Show)

data CleanupAction = CleanupAction
  { cleanupActionLabel :: String
  , cleanupActionState :: MVar (Maybe (IO ()))
  }

data SlotState
  = SlotIdle
  | SlotAcquired FrameOwnership
  | SlotSubmitted FrameOwnership Word64 [CleanupAction]

data SlotStateView
  = SlotStateIdle
  | SlotStateAcquired FrameOwnership
  | SlotStateSubmitted FrameOwnership Word64
  deriving stock (Eq, Show)

data RenderFinishedState
  = RenderFinishedIdle
  | RenderFinishedSignalSubmitted Word64
  | RenderFinishedPresentWaitQueued Word64
  deriving stock (Eq, Show)

data AcquireDriverOutcome
  = AcquireDriverSuccess Word32
  | AcquireDriverSuboptimal Word32
  | AcquireDriverTimeout
  | AcquireDriverNotReady
  | AcquireDriverOutOfDate
  | AcquireDriverSurfaceLost
  | AcquireDriverDeviceLost
  | AcquireDriverUnexpected Result.Result
  deriving stock (Eq, Show)

data PresentDriverOutcome
  = PresentDriverSuccess
  | PresentDriverSuboptimal
  | PresentDriverOutOfDate
  | PresentDriverSurfaceLost
  | PresentDriverDeviceLost
  | PresentDriverAcceptedFailure Result.Result
  | PresentDriverRejected Result.Result
  | PresentDriverAmbiguous Result.Result
  deriving stock (Eq, Show)

data AcquireOutcome
  = AcquireReady
      { acquiredFrameSlot :: FrameSlot
      , acquiredGeneration :: Generation
      , acquiredGenerationImage :: GenerationImage
      , acquireRecreateAfterPresent :: Bool
      }
  | AcquireDeferredNow DeferredReason
  | AcquireNeedsRecreation

data QueuePresentOutcome
  = QueuePresentComplete {queuePresentSuboptimal :: Bool}
  | QueuePresentNeedsRecreation
  deriving stock (Eq, Show)

data FrameSlot = FrameSlot
  { frameSlotIndex :: Int
  , frameSlotImageAvailable :: Vk.Semaphore
  , frameSlotCommandPool :: Vk.CommandPool
  , frameSlotCommandBuffer :: Vk.CommandBuffer
  , frameSlotState :: MVar SlotState
  , frameSlotPoolCleanup :: CleanupAction
  , frameSlotImageAvailableCleanup :: CleanupAction
  , frameSlotDescriptorStorage :: MVar DescriptorFrameStorage
  }

data GenerationImage = GenerationImage
  { generationImageIndex :: Word32
  , generationImageGeneration :: Lifetime.ResourceGeneration
  , generationImageHandle :: Vk.Image
  , generationImageView :: Vk.ImageView
  , generationImageState :: ImageState
  , generationColorTarget :: Pipeline.ColorImage 'B8G8R8A8Srgb
  , generationRenderFinished :: Vk.Semaphore
  , generationRenderFinishedState :: MVar RenderFinishedState
  , generationImageViewCleanup :: CleanupAction
  , generationRenderFinishedCleanup :: CleanupAction
  }

data Generation = Generation
  { generationNumber :: Word64
  , generationHandle :: Extensions.SwapchainKHR
  , generationExtent :: (Word32, Word32)
  , generationLifetime :: LifetimeGate
  , generationImages :: Vector GenerationImage
  , generationOwner :: GenerationOwner
  }

newtype GenerationOwner = GenerationOwner
  { generationOwnerCleanup :: MVar [CleanupAction]
  }

data RawGeneration = RawGeneration
  { rawGenerationNumber :: Word64
  , rawGenerationHandle :: Extensions.SwapchainKHR
  , rawGenerationExtent :: Fundamental.Extent2D
  , rawGenerationLifetime :: LifetimeGate
  , rawGenerationOwner :: GenerationOwner
  }

data GenerationPreflight
  = GenerationMinimized Word64
  | GenerationReady GenerationCreatePlan

data GenerationCreatePlan = GenerationCreatePlan
  { generationPlanNumber :: Word64
  , generationPlanExtent :: Fundamental.Extent2D
  , generationPlanCreateInfo :: KHR.SwapchainCreateInfoKHR '[]
  }

data RuntimeState = RuntimeState
  { runtimeGeneration :: Maybe Generation
  , runtimeRetiredGenerations :: [Generation]
  , runtimePartialGeneration :: Maybe RawGeneration
  , runtimeExtent :: (Word32, Word32)
  , runtimeNextGeneration :: Word64
  , runtimeNextSlot :: Int
  , runtimeRecreationPending :: Bool
  }

data CoreState a = CoreActive a | CoreRetiring a | CoreReleased | CorePoisoned a
  deriving stock (Eq, Show)

activeCoreState :: CoreState a -> Either VpipeError a
activeCoreState state = case state of
  CoreActive value -> Right value
  CoreRetiring _ -> Left SwapchainReleased
  CoreReleased -> Left SwapchainReleased
  CorePoisoned _ -> Left SwapchainPoisoned

poisonCoreState :: CoreState a -> CoreState a
poisonCoreState state = case state of
  CoreActive value -> CorePoisoned value
  CoreRetiring value -> CoreRetiring value
  CorePoisoned value -> CorePoisoned value
  CoreReleased -> CoreReleased

releaseCoreState :: CoreState a -> (CoreState a, Maybe a)
releaseCoreState state = case state of
  CoreActive value -> (CoreRetiring value, Just value)
  CoreRetiring value -> (CoreRetiring value, Just value)
  CoreReleased -> (CoreReleased, Nothing)
  CorePoisoned value -> (CoreRetiring value, Just value)

data Swapchain = Swapchain
  { swapchainContext :: Context
  , swapchainSurface :: Surface
  , swapchainConfig :: SwapchainConfig
  , swapchainFrameDomain :: FrameDomain
  , swapchainSlots :: Vector FrameSlot
  , swapchainState :: MVar (CoreState RuntimeState)
  , swapchainOperationLock :: MVar ()
  }

newtype LockedSwapchain = LockedSwapchain Swapchain

newtype FrameDomain = FrameDomain Unique
  deriving stock (Eq)

withSwapchainOperation :: Swapchain -> (LockedSwapchain -> IO a) -> IO a
withSwapchainOperation swapchain action =
  withContextLease (swapchainContext swapchain) (withSwapchainOperationLeased swapchain action)

withSwapchainOperationLeased :: Swapchain -> (LockedSwapchain -> IO a) -> IO a
withSwapchainOperationLeased swapchain action =
  withOperationMutexForTest (swapchainOperationLock swapchain) (action (LockedSwapchain swapchain))

{- | Context access for a caller already holding the swapchain operation lock
and its outer lifecycle lease.
-}
lockedSwapchainContext :: LockedSwapchain -> Context
lockedSwapchainContext (LockedSwapchain swapchain) = swapchainContext swapchain

lockedSwapchainFrameDomain :: LockedSwapchain -> FrameDomain
lockedSwapchainFrameDomain (LockedSwapchain swapchain) = swapchainFrameDomain swapchain

lockedSwapchainFrameConfiguration :: LockedSwapchain -> IO (Context, FrameDomain, Int)
lockedSwapchainFrameConfiguration locked@(LockedSwapchain swapchain) = do
  state <- readMVar (swapchainState swapchain)
  _ <- either throwIO pure (activeCoreState state)
  pure (lockedSwapchainContext locked, lockedSwapchainFrameDomain locked, Vector.length (swapchainSlots swapchain))

lockedSwapchainExtent :: LockedSwapchain -> IO (Word32, Word32)
lockedSwapchainExtent (LockedSwapchain swapchain) = do
  state <- readMVar (swapchainState swapchain)
  runtimeExtent <$> either throwIO pure (activeCoreState state)

withOperationMutexForTest :: MVar () -> IO a -> IO a
withOperationMutexForTest mutex = withMVar mutex . const

slotStateView :: SlotState -> SlotStateView
slotStateView state = case state of
  SlotIdle -> SlotStateIdle
  SlotAcquired ownership -> SlotStateAcquired ownership
  SlotSubmitted ownership timeline _ -> SlotStateSubmitted ownership timeline

acquireSlotTransition :: FrameOwnership -> SlotStateView -> Either VpipeError SlotStateView
acquireSlotTransition ownership state = case state of
  SlotStateIdle -> Right (SlotStateAcquired ownership)
  _ -> slotTransitionFailure "claim acquired swapchain slot" state

submitSlotTransition :: Word64 -> SlotStateView -> Either VpipeError SlotStateView
submitSlotTransition timeline state = case state of
  SlotStateAcquired ownership -> Right (SlotStateSubmitted ownership timeline)
  _ -> slotTransitionFailure "publish swapchain slot submission" state

completeSlotTransition :: SlotStateView -> Either VpipeError SlotStateView
completeSlotTransition state = case state of
  SlotStateSubmitted _ _ -> Right SlotStateIdle
  _ -> slotTransitionFailure "retire submitted swapchain slot" state

submitRenderFinishedTransition :: Word64 -> RenderFinishedState -> Either VpipeError RenderFinishedState
submitRenderFinishedTransition timeline state = case state of
  RenderFinishedIdle -> Right (RenderFinishedSignalSubmitted timeline)
  _ -> renderFinishedTransitionFailure "submit render-finished signal" state

queuePresentWaitTransition :: RenderFinishedState -> Either VpipeError RenderFinishedState
queuePresentWaitTransition state = case state of
  RenderFinishedSignalSubmitted timeline -> Right (RenderFinishedPresentWaitQueued timeline)
  _ -> renderFinishedTransitionFailure "queue render-finished present wait" state

reacquireRenderFinishedTransition :: RenderFinishedState -> Either VpipeError RenderFinishedState
reacquireRenderFinishedTransition state = case state of
  RenderFinishedIdle -> Right RenderFinishedIdle
  RenderFinishedPresentWaitQueued _ -> Right RenderFinishedIdle
  _ -> renderFinishedTransitionFailure "reacquire render-finished image" state

slotTransitionFailure :: String -> SlotStateView -> Either VpipeError a
slotTransitionFailure operation state = Left (VulkanFailure operation ("invalid slot state " <> show state))

renderFinishedTransitionFailure :: String -> RenderFinishedState -> Either VpipeError a
renderFinishedTransitionFailure operation state = Left (VulkanFailure operation ("invalid binary semaphore state " <> show state))

classifyAcquireDriverResult :: Result.Result -> Word32 -> AcquireDriverOutcome
classifyAcquireDriverResult result imageIndex = case result of
  Result.SUCCESS -> AcquireDriverSuccess imageIndex
  Result.SUBOPTIMAL_KHR -> AcquireDriverSuboptimal imageIndex
  Result.TIMEOUT -> AcquireDriverTimeout
  Result.NOT_READY -> AcquireDriverNotReady
  Result.ERROR_OUT_OF_DATE_KHR -> AcquireDriverOutOfDate
  Result.ERROR_SURFACE_LOST_KHR -> AcquireDriverSurfaceLost
  Result.ERROR_DEVICE_LOST -> AcquireDriverDeviceLost
  _ -> AcquireDriverUnexpected result

classifyPresentDriverResult :: Result.Result -> PresentDriverOutcome
classifyPresentDriverResult result = case result of
  Result.SUCCESS -> PresentDriverSuccess
  Result.SUBOPTIMAL_KHR -> PresentDriverSuboptimal
  Result.ERROR_OUT_OF_DATE_KHR -> PresentDriverOutOfDate
  Result.ERROR_SURFACE_LOST_KHR -> PresentDriverSurfaceLost
  Result.ERROR_DEVICE_LOST -> PresentDriverDeviceLost
  Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT -> PresentDriverAcceptedFailure result
  Result.ERROR_OUT_OF_HOST_MEMORY -> PresentDriverRejected result
  Result.ERROR_OUT_OF_DEVICE_MEMORY -> PresentDriverRejected result
  _ -> PresentDriverAmbiguous result

presentDriverWaitAccepted :: PresentDriverOutcome -> Bool
presentDriverWaitAccepted outcome = case outcome of
  PresentDriverSuccess -> True
  PresentDriverSuboptimal -> True
  PresentDriverOutOfDate -> True
  PresentDriverSurfaceLost -> True
  PresentDriverAcceptedFailure _ -> True
  PresentDriverDeviceLost -> False
  PresentDriverRejected _ -> False
  PresentDriverAmbiguous _ -> False

{- | Acquires are deliberately bounded so a lost or permanently hidden
surface cannot make one frame call unresponsive for an effectively
infinite duration. User values above five seconds are capped.
-}
maximumAcquireTimeoutNanoseconds :: Word64
maximumAcquireTimeoutNanoseconds = 5_000_000_000

finiteAcquireTimeoutNanoseconds :: Word64 -> Word64
finiteAcquireTimeoutNanoseconds = min maximumAcquireTimeoutNanoseconds

validateAcquiredImageIndex :: Int -> Word32 -> Either VpipeError Int
validateAcquiredImageIndex imageCount imageIndex
  | toInteger imageIndex < toInteger imageCount = Right (fromIntegral imageIndex)
  | otherwise =
      Left
        ( VulkanFailure
            "vkAcquireNextImageKHR"
            ("returned image index " <> show imageIndex <> " for a generation with " <> show imageCount <> " images")
        )

data AcquireDriverCall
  = AcquireDriverCallKnown AcquireDriverOutcome
  | AcquireDriverCallAmbiguous SomeException

data PresentDriverCall
  = PresentDriverCallKnown PresentDriverOutcome
  | PresentDriverCallAmbiguous SomeException

runAcquireDriverCall :: IO (Result.Result, Word32) -> IO AcquireDriverCall
runAcquireDriverCall action = do
  result <- trySynchronous action
  case result of
    Right (driverResult, imageIndex) -> pure (AcquireDriverCallKnown (classifyAcquireDriverResult driverResult imageIndex))
    Left error' -> case fromException error' of
      Just vulkanError -> pure (AcquireDriverCallKnown (classifyAcquireDriverResult (Vulkan.vulkanExceptionResult vulkanError) 0))
      Nothing -> pure (AcquireDriverCallAmbiguous error')

runPresentDriverCall :: IO Result.Result -> IO PresentDriverCall
runPresentDriverCall action = do
  result <- trySynchronous action
  case result of
    Right driverResult -> pure (PresentDriverCallKnown (classifyPresentDriverResult driverResult))
    Left error' -> case fromException error' of
      Just vulkanError -> pure (PresentDriverCallKnown (classifyPresentDriverResult (Vulkan.vulkanExceptionResult vulkanError)))
      Nothing -> pure (PresentDriverCallAmbiguous error')

runAcquireDriverCallForTest :: IO (Result.Result, Word32) -> IO (Either String AcquireDriverOutcome)
runAcquireDriverCallForTest action = do
  result <- runAcquireDriverCall action
  pure $ case result of
    AcquireDriverCallKnown outcome -> Right outcome
    AcquireDriverCallAmbiguous error' -> Left (show error')

runPresentDriverCallForTest :: IO Result.Result -> IO (Either String PresentDriverOutcome)
runPresentDriverCallForTest action = do
  result <- runPresentDriverCall action
  pure $ case result of
    PresentDriverCallKnown outcome -> Right outcome
    PresentDriverCallAmbiguous error' -> Left (show error')

{- | Select and prepare the next slot while the swapchain-wide protocol lock is
held. A minimized swapchain has no generation and therefore no claimable slot.
-}
claimFrameSlotLocked :: LockedSwapchain -> IO (Maybe (FrameSlot, Generation))
claimFrameSlotLocked locked@(LockedSwapchain swapchain) = do
  state <- readMVar (swapchainState swapchain)
  runtime <- either throwIO pure (activeCoreState state)
  case runtimeGeneration runtime of
    Nothing -> pure Nothing
    Just generation -> do
      let slots = swapchainSlots swapchain
          slotCount = Vector.length slots
          slotIndex = runtimeNextSlot runtime `mod` slotCount
          slot = slots Vector.! slotIndex
      prepareFrameSlot locked slot
      modifyMVarMasked (swapchainState swapchain) $ \current -> do
        active <- either throwIO pure (activeCoreState current)
        pure (CoreActive active{runtimeNextSlot = (slotIndex + 1) `mod` slotCount}, ())
      pure (Just (slot, generation))

{- | Prepare a frame slot and acquire its next image using the configured
finite timeout. A successful or suboptimal acquire publishes 'SlotAcquired'
before returning. Suboptimal acquisition must still be submitted and
presented before the caller recreates the generation.
-}
acquireNextImageLocked :: LockedSwapchain -> IO AcquireOutcome
acquireNextImageLocked locked@(LockedSwapchain swapchain) = do
  state <- readMVar (swapchainState swapchain)
  runtime <- either throwIO pure (activeCoreState state)
  providerExtent <- sequenceA (surfaceFramebufferExtent (swapchainSurface swapchain))
  let providerMatchesRuntime = case providerExtent of
        Just (width, height) ->
          width > 0
            && height > 0
            && toInteger width == toInteger (fst (runtimeExtent runtime))
            && toInteger height == toInteger (snd (runtimeExtent runtime))
        Nothing -> True
  extentChoice <-
    if providerMatchesRuntime
      then pure Nothing
      else do
        let context = swapchainContext swapchain
            surface = swapchainSurface swapchain
        capabilities <-
          mapVulkan
            "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
            (Surface.getPhysicalDeviceSurfaceCapabilitiesKHR (contextPhysicalDevice context) (surfaceHandle surface))
        Just
          <$> either
            throwIO
            pure
            ( chooseExtent
                (Surface.currentExtent capabilities)
                (Surface.minImageExtent capabilities)
                (Surface.maxImageExtent capabilities)
                providerExtent
            )
  case extentChoice of
    Just ExtentMinimized -> pure (AcquireDeferredNow FramebufferMinimized)
    Just (ExtentReady extent)
      | (extentWidth extent, extentHeight extent) /= runtimeExtent runtime -> pure AcquireNeedsRecreation
    _ -> do
      claimed <- claimFrameSlotLocked locked
      case claimed of
        Nothing -> pure (AcquireDeferredNow FramebufferMinimized)
        Just (slot, generation) -> mask_ $ do
          let acquire =
                KHR.acquireNextImageKHR
                  (contextDevice (swapchainContext swapchain))
                  (generationHandle generation)
                  (finiteAcquireTimeoutNanoseconds (acquireTimeoutNanoseconds (swapchainConfig swapchain)))
                  (frameSlotImageAvailable slot)
                  zero
          driverCall <- runAcquireDriverCall acquire
          case driverCall of
            AcquireDriverCallAmbiguous error' -> do
              retainAcquiredSlot slot (uncertainAcquireOwnership generation)
              poisonAndThrow locked error'
            AcquireDriverCallKnown outcome -> resolveAcquireOutcome locked slot generation outcome

resolveAcquireOutcome :: LockedSwapchain -> FrameSlot -> Generation -> AcquireDriverOutcome -> IO AcquireOutcome
resolveAcquireOutcome locked slot generation outcome = case outcome of
  AcquireDriverSuccess imageIndex -> publishAcquiredImage False imageIndex
  AcquireDriverSuboptimal imageIndex -> publishAcquiredImage True imageIndex
  AcquireDriverTimeout -> pure (AcquireDeferredNow AcquireTimedOut)
  AcquireDriverNotReady -> pure (AcquireDeferredNow AcquireTimedOut)
  AcquireDriverOutOfDate -> pure AcquireNeedsRecreation
  AcquireDriverSurfaceLost -> poisonAndThrow locked SurfaceLost
  AcquireDriverDeviceLost -> poisonAndThrow locked DeviceLost
  AcquireDriverUnexpected result -> do
    retainAcquiredSlot slot (uncertainAcquireOwnership generation)
    poisonAndThrow locked (VulkanFailure "vkAcquireNextImageKHR" (show result))
 where
  publishAcquiredImage suboptimal imageIndex =
    case generationImageAt generation imageIndex of
      Nothing ->
        do
          retainAcquiredSlot slot (FrameOwnership (generationNumber generation) imageIndex)
          poisonAndThrow
            locked
            ( VulkanFailure
                "vkAcquireNextImageKHR"
                ( "returned image index "
                    <> show imageIndex
                    <> " for a generation with "
                    <> show (Vector.length (generationImages generation))
                    <> " images"
                )
            )
      Just image -> do
        publication <- trySynchronous (publishSlotAcquiredLocked locked slot generation image)
        case publication of
          Left error' -> do
            retainAcquiredSlot slot (FrameOwnership (generationNumber generation) imageIndex)
            poisonAndThrow locked error'
          Right _ -> pure (AcquireReady slot generation image suboptimal)

uncertainAcquireOwnership :: Generation -> FrameOwnership
uncertainAcquireOwnership generation = FrameOwnership (generationNumber generation) maxBound

retainAcquiredSlot :: FrameSlot -> FrameOwnership -> IO ()
retainAcquiredSlot slot = retainAcquiredState (frameSlotState slot)

retainAcquiredState :: MVar SlotState -> FrameOwnership -> IO ()
retainAcquiredState stateVariable ownership =
  modifyMVarMasked stateVariable $ \state -> case state of
    SlotIdle -> pure (SlotAcquired ownership, ())
    _ -> pure (state, ())

generationImageAt :: Generation -> Word32 -> Maybe GenerationImage
generationImageAt generation imageIndex = do
  index <- either (const Nothing) Just (validateAcquiredImageIndex (Vector.length images) imageIndex)
  images Vector.!? index
 where
  images = generationImages generation

{- | Publish a successful image acquisition. The image's render-finished
semaphore only becomes reusable here when Vulkan has returned that same image,
which proves a previously queued present wait has consumed its signal.
-}
publishSlotAcquiredLocked :: LockedSwapchain -> FrameSlot -> Generation -> GenerationImage -> IO FrameOwnership
publishSlotAcquiredLocked locked slot generation image = mask_ $ do
  ensureSlotAndImageOwned locked slot generation image
  let ownership = FrameOwnership (generationNumber generation) (generationImageIndex image)
  currentSlot <- readMVar (frameSlotState slot)
  currentRender <- readMVar (generationRenderFinishedState image)
  _ <- either throwIO pure (acquireSlotTransition ownership (slotStateView currentSlot))
  nextRender <- either throwIO pure (reacquireRenderFinishedTransition currentRender)
  modifyMVarMasked (frameSlotState slot) $ \_ -> do
    modifyMVarMasked (generationRenderFinishedState image) (const (pure (nextRender, ())))
    pure (SlotAcquired ownership, ())
  pure ownership

{- | Atomically publish the state owned after an accepted graphics submission.
The slot timeline and all idempotent retained releases are one state update;
the render-finished signal state is updated under the same protocol lock.
-}
publishAcceptedSubmissionLocked :: LockedSwapchain -> FrameSlot -> GenerationImage -> Word64 -> [IO ()] -> IO ()
publishAcceptedSubmissionLocked locked slot image timeline releases = mask_ $ do
  ownership <- ensureSubmittedImageOwned locked slot image
  currentSlot <- readMVar (frameSlotState slot)
  currentRender <- readMVar (generationRenderFinishedState image)
  _ <- either throwIO pure (submitSlotTransition timeline (slotStateView currentSlot))
  nextRender <- either throwIO pure (submitRenderFinishedTransition timeline currentRender)
  actions <- newCleanupActions "frame retained release" releases
  modifyMVarMasked (frameSlotState slot) $ \_ -> do
    modifyMVarMasked (generationRenderFinishedState image) (const (pure (nextRender, ())))
    pure (SlotSubmitted ownership timeline actions, ())

{- | Test seam for exercising the acquire/present synchronization chain with
an already-recorded command buffer. High-level Frame submission will own the
general resource/dependency path.
-}
submitPreparedFrameForTest :: LockedSwapchain -> FrameSlot -> GenerationImage -> IO Word64
submitPreparedFrameForTest locked@(LockedSwapchain swapchain) slot image = mask_ $ do
  let queue = graphicsQueue (swapchainContext swapchain)
  timeline <-
    submitCommandBuffersWithLeased
      queue
      []
      [BinarySemaphoreWait (frameSlotImageAvailable slot) Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT]
      [BinarySemaphoreSignal (generationRenderFinished image) Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT]
      (Vector.singleton (frameSlotCommandBuffer slot))
  publishAcceptedSubmissionLocked locked slot image timeline []
  pure timeline

{- | Publish the explicit recovery submission which consumes an outstanding
acquire semaphore without signaling an image render-finished semaphore.
-}
publishAcquireRecoveryLocked :: LockedSwapchain -> FrameSlot -> Word64 -> [IO ()] -> IO ()
publishAcquireRecoveryLocked locked slot timeline releases = mask_ $ do
  ensureSlotOwned locked slot
  current <- readMVar (frameSlotState slot)
  _ <- either throwIO pure (submitSlotTransition timeline (slotStateView current))
  ownership <- case current of
    SlotAcquired value -> pure value
    _ -> throwIO (VulkanFailure "publish acquire recovery" "slot was not acquired")
  actions <- newCleanupActions "frame recovery retained release" releases
  modifyMVarMasked (frameSlotState slot) (const (pure (SlotSubmitted ownership timeline actions, ())))

publishPresentWaitLocked :: LockedSwapchain -> Generation -> GenerationImage -> IO ()
publishPresentWaitLocked locked generation image = mask_ $ do
  ensureImageOwned locked generation image
  advancePresentWaitState (generationRenderFinishedState image)

advancePresentWaitState :: MVar RenderFinishedState -> IO ()
advancePresentWaitState stateVariable =
  modifyMVarMasked stateVariable $ \current -> do
    next <- either throwIO pure (queuePresentWaitTransition current)
    pure (next, ())

publishPresentDriverOutcomeForTest :: MVar RenderFinishedState -> PresentDriverOutcome -> IO ()
publishPresentDriverOutcomeForTest stateVariable outcome =
  when (presentDriverWaitAccepted outcome) (advancePresentWaitState stateVariable)

{- | Queue presentation while holding the present queue's external
synchronization lock. Vulkan specifies that OUT_OF_DATE, SURFACE_LOST, and
SUBOPTIMAL presentation calls still enqueue the semaphore wait, so those
paths publish 'RenderFinishedPresentWaitQueued' before returning or throwing.
-}
presentGenerationImageLocked :: LockedSwapchain -> Generation -> GenerationImage -> IO QueuePresentOutcome
presentGenerationImageLocked locked@(LockedSwapchain swapchain) generation image = mask_ $ do
  ensureImageOwned locked generation image
  renderState <- readMVar (generationRenderFinishedState image)
  _ <- either throwIO pure (queuePresentWaitTransition renderState)
  let presentInfo =
        KHR.PresentInfoKHR
          { KHR.next = ()
          , KHR.waitSemaphores = Vector.singleton (generationRenderFinished image)
          , KHR.swapchains = Vector.singleton (generationHandle generation)
          , KHR.imageIndices = Vector.singleton (generationImageIndex image)
          , KHR.results = nullPtr
          }
  withQueueHandleLockedLeased (surfacePresentQueue (swapchainSurface swapchain)) $ \queue -> do
    driverCall <- runPresentDriverCall (KHR.queuePresentKHR queue presentInfo)
    resolvePresentOutcome locked generation image driverCall

resolvePresentOutcome :: LockedSwapchain -> Generation -> GenerationImage -> PresentDriverCall -> IO QueuePresentOutcome
resolvePresentOutcome locked generation image driverCall = case driverCall of
  PresentDriverCallAmbiguous error' -> poisonAndThrow locked error'
  PresentDriverCallKnown outcome -> do
    when (presentDriverWaitAccepted outcome) $ do
      publication <- trySynchronous (publishPresentWaitLocked locked generation image)
      case publication of
        Left error' -> poisonAndThrow locked error'
        Right () -> pure ()
    case outcome of
      PresentDriverSuccess -> pure (QueuePresentComplete False)
      PresentDriverSuboptimal -> pure (QueuePresentComplete True)
      PresentDriverOutOfDate -> pure QueuePresentNeedsRecreation
      PresentDriverSurfaceLost -> poisonAndThrow locked SurfaceLost
      PresentDriverDeviceLost -> poisonAndThrow locked DeviceLost
      PresentDriverAcceptedFailure result -> poisonAndThrow locked (VulkanFailure "vkQueuePresentKHR" (show result))
      PresentDriverRejected result -> poisonAndThrow locked (VulkanFailure "vkQueuePresentKHR" (show result))
      PresentDriverAmbiguous result -> poisonAndThrow locked (VulkanFailure "vkQueuePresentKHR" (show result))

poisonAndThrow :: (Exception e) => LockedSwapchain -> e -> IO a
poisonAndThrow locked error' = do
  _ <- trySynchronous (poisonSwapchainLocked locked)
  throwIO error'

poisonSwapchainLocked :: LockedSwapchain -> IO ()
poisonSwapchainLocked (LockedSwapchain swapchain) = mask_ $ do
  modifyMVarMasked (swapchainState swapchain) $ \case
    CoreActive runtime -> pure (CorePoisoned runtime, ())
    CorePoisoned runtime -> pure (CorePoisoned runtime, ())
    CoreRetiring _ -> throwIO SwapchainReleased
    CoreReleased -> throwIO SwapchainReleased

inspectSlotStateLocked :: LockedSwapchain -> FrameSlot -> IO SlotStateView
inspectSlotStateLocked locked slot = do
  ensureSlotOwned locked slot
  slotStateView <$> readMVar (frameSlotState slot)

{- | Lazily allocate one descriptor frame per slot and descriptor layout.
The storage is reset only by 'prepareFrameSlot', after that slot's prior
timeline value has completed.
-}
descriptorFrameForSlotLocked :: LockedSwapchain -> FrameSlot -> DescriptorLayout -> IO DescriptorFrame
descriptorFrameForSlotLocked locked slot layout = do
  ensureSlotOwned locked slot
  lookupOrCreateSlotValueForTest
    (frameSlotDescriptorStorage slot)
    (descriptorLayoutIdentity layout)
    (newDescriptorFrameLeased layout)

ensureSlotOwned :: LockedSwapchain -> FrameSlot -> IO ()
ensureSlotOwned (LockedSwapchain swapchain) slot = do
  let index = frameSlotIndex slot
      slots = swapchainSlots swapchain
  when (index < 0 || index >= Vector.length slots || frameSlotCommandPool (slots Vector.! index) /= frameSlotCommandPool slot) $
    throwIO (VulkanFailure "swapchain slot ownership" "slot does not belong to this swapchain")

ensureImageOwned :: LockedSwapchain -> Generation -> GenerationImage -> IO ()
ensureImageOwned (LockedSwapchain swapchain) generation image = do
  state <- readMVar (swapchainState swapchain)
  runtime <- either throwIO pure (activeCoreState state)
  let index = fromIntegral (generationImageIndex image)
      matching = do
        active <- runtimeGeneration runtime
        candidate <- generationImages active Vector.!? index
        pure
          ( generationNumber active == generationNumber generation
              && generationHandle active == generationHandle generation
              && generationImageHandle candidate == generationImageHandle image
          )
  unless (matching == Just True) (throwIO FrameExpired)

ensureSlotAndImageOwned :: LockedSwapchain -> FrameSlot -> Generation -> GenerationImage -> IO ()
ensureSlotAndImageOwned locked slot generation image = ensureSlotOwned locked slot >> ensureImageOwned locked generation image

ensureSubmittedImageOwned :: LockedSwapchain -> FrameSlot -> GenerationImage -> IO FrameOwnership
ensureSubmittedImageOwned locked@(LockedSwapchain swapchain) slot image = do
  ensureSlotOwned locked slot
  state <- readMVar (swapchainState swapchain)
  runtime <- either throwIO pure (activeCoreState state)
  generation <- maybe (throwIO FrameExpired) pure (runtimeGeneration runtime)
  ensureImageOwned locked generation image
  let ownership = FrameOwnership (generationNumber generation) (generationImageIndex image)
  current <- slotStateView <$> readMVar (frameSlotState slot)
  case current of
    SlotStateAcquired acquired
      | acquired == ownership -> pure ownership
    _ -> throwIO (VulkanFailure "publish accepted swapchain submission" ("slot/image ownership mismatch in " <> show current))

newSwapchain :: Context -> Surface -> SwapchainConfig -> IO Swapchain
newSwapchain context surface config = withContextLease context $ mask $ \_ -> do
  either throwIO pure (validateFramesInFlight (framesInFlight config))
  unless (contextOwnsSurface context surface) (throwIO SurfaceContextMismatch)
  slots <- createFrameSlots context (framesInFlight config)
  state <- newMVar (CoreActive emptyRuntimeState)
  operationLock <- newMVar ()
  domain <- FrameDomain <$> newUnique
  let swapchain = Swapchain context surface config domain slots state operationLock
      releaseAfterContextIdle = withSwapchainOperationLeased swapchain releaseSwapchainAfterContextIdleLocked
      releaseFailedCreation = withSwapchainOperationLeased swapchain (releaseSwapchainLockedWith True)
  registerContextFinalizerLeased context releaseAfterContextIdle `onException` destroyFrameSlots context slots
  creation <- try (withSwapchainOperationLeased swapchain (replaceGenerationLockedWith False))
  case creation of
    Left (primary :: SomeException) -> do
      _ <- try releaseFailedCreation :: IO (Either SomeException ())
      throwIO primary
    Right _ -> pure swapchain

destroySwapchain :: Swapchain -> IO ()
destroySwapchain swapchain =
  withContextLease (swapchainContext swapchain) $
    withSwapchainOperationLeased swapchain releaseSwapchainLocked

swapchainExtent :: Swapchain -> IO (Word32, Word32)
swapchainExtent swapchain =
  withContextLease (swapchainContext swapchain) $
    withSwapchainOperationLeased swapchain $ \(LockedSwapchain locked) -> do
      state <- readMVar (swapchainState locked)
      runtimeExtent <$> either throwIO pure (activeCoreState state)

replaceGeneration :: Swapchain -> IO Bool
replaceGeneration swapchain =
  withContextLease (swapchainContext swapchain) $
    withSwapchainOperationLeased swapchain replaceGenerationLocked

replaceGenerationLocked :: LockedSwapchain -> IO Bool
replaceGenerationLocked = replaceGenerationLockedWith True

replaceGenerationLockedWith :: Bool -> LockedSwapchain -> IO Bool
replaceGenerationLockedWith recreation (LockedSwapchain swapchain) = mask_ $ do
  cleanupPartialGeneration swapchain
  cleanupRetiredGenerations swapchain
  previousState <- readMVar (swapchainState swapchain)
  previous <- either throwIO pure (activeCoreState previousState)
  preflight <- preflightGeneration swapchain previous
  case preflight of
    GenerationMinimized number ->
      modifyMVarMasked (swapchainState swapchain) $ \state -> do
        current <- either throwIO pure (activeCoreState state)
        pure (CoreActive (publishRetirement recreation number current), ())
    GenerationReady plan ->
      void $
        runIrreversibleRecreationForTest
          (swapchainState swapchain)
          (const (pure plan))
          (\runtime readyPlan -> publishRetirement recreation (generationPlanNumber readyPlan) runtime)
          (createRawGeneration (swapchainContext swapchain))
          publishPartialGeneration
          (buildRawGeneration (swapchainContext swapchain))
          finishRawGeneration
  cleanupRetiredGenerations swapchain
  pure (recreation && isJust (runtimeGeneration previous))

takeRecreationNotificationLocked :: LockedSwapchain -> IO Bool
takeRecreationNotificationLocked (LockedSwapchain swapchain) =
  modifyMVarMasked (swapchainState swapchain) $ \state -> do
    runtime <- either throwIO pure (activeCoreState state)
    pure (CoreActive runtime{runtimeRecreationPending = False}, runtimeRecreationPending runtime)

{- | Run a recreation with an explicit irreversible boundary. All work in
@preflight@ is reversible. Immediately before @createNative@, @retireOld@
is published under 'CorePoisoned', because Vulkan retires a non-null
@oldSwapchain@ as soon as the native call is made even when it fails.

Native/build failures return the retained ownership state to 'CoreActive':
the former generation remains retired, no generation is active, and a later
preflight therefore selects @VK_NULL_HANDLE@.
-}
runIrreversibleRecreationForTest :: MVar (CoreState state) -> (state -> IO plan) -> (state -> plan -> state) -> (plan -> IO raw) -> (state -> raw -> state) -> (raw -> IO ready) -> (state -> raw -> ready -> state) -> IO ready
runIrreversibleRecreationForTest stateVariable preflight retireOld createNative publishRaw build finish = mask_ $ do
  initial <- readMVar stateVariable >>= either throwIO pure . activeCoreState
  plan <- preflight initial
  modifyMVarMasked stateVariable $ \state -> do
    current <- either throwIO pure (activeCoreState state)
    pure (CorePoisoned (retireOld current plan), ())
  nativeResult <- try (createNative plan)
  raw <- case nativeResult of
    Left (primary :: SomeException) -> reactivateAndThrow stateVariable primary
    Right value -> pure value
  modifyMVarMasked stateVariable $ \case
    CorePoisoned owned -> pure (CorePoisoned (publishRaw owned raw), ())
    _ -> throwIO (VulkanFailure "swapchain recreation" "native creation lost poisoned ownership")
  buildResult <- try (build raw)
  case buildResult of
    Left (primary :: SomeException) -> reactivateAndThrow stateVariable primary
    Right ready -> do
      modifyMVarMasked stateVariable $ \case
        CorePoisoned owned -> pure (CoreActive (finish owned raw ready), ())
        _ -> throwIO (VulkanFailure "swapchain recreation" "generation build lost poisoned ownership")
      pure ready

activateRetainedState :: MVar (CoreState state) -> IO ()
activateRetainedState stateVariable =
  modifyMVarMasked stateVariable $ \case
    CorePoisoned owned -> pure (CoreActive owned, ())
    _ -> throwIO (VulkanFailure "swapchain recreation" "failure recovery lost retained ownership")

reactivateAndThrow :: MVar (CoreState state) -> SomeException -> IO a
reactivateAndThrow stateVariable primary = do
  _ <- try (activateRetainedState stateVariable) :: IO (Either SomeException ())
  throwIO primary

emptyRuntimeState :: RuntimeState
emptyRuntimeState = RuntimeState Nothing [] Nothing (0, 0) 0 0 False

publishRetirement :: Bool -> Word64 -> RuntimeState -> RuntimeState
publishRetirement recreation number runtime =
  runtime
    { runtimeGeneration = Nothing
    , runtimeRetiredGenerations = maybe id (:) (runtimeGeneration runtime) (runtimeRetiredGenerations runtime)
    , runtimePartialGeneration = Nothing
    , runtimeExtent = (0, 0)
    , runtimeNextGeneration = number + 1
    , runtimeRecreationPending = runtimeRecreationPending runtime || recreation
    }

publishPartialGeneration :: RuntimeState -> RawGeneration -> RuntimeState
publishPartialGeneration runtime raw = runtime{runtimePartialGeneration = Just raw}

finishRawGeneration :: RuntimeState -> RawGeneration -> Generation -> RuntimeState
finishRawGeneration runtime _ generation =
  runtime
    { runtimeGeneration = Just generation
    , runtimePartialGeneration = Nothing
    , runtimeExtent = generationExtent generation
    }

releaseSwapchainLocked :: LockedSwapchain -> IO ()
releaseSwapchainLocked = releaseSwapchainLockedWith False

-- Context finalizers run only after the lifecycle gate has closed and
-- vkDeviceWaitIdle has completed (or reported device loss). At that boundary
-- it is safe to force-drain ownership retained by an ambiguous submission.
releaseSwapchainAfterContextIdleLocked :: LockedSwapchain -> IO ()
releaseSwapchainAfterContextIdleLocked = releaseSwapchainLockedWith True

releaseSwapchainLockedWith :: Bool -> LockedSwapchain -> IO ()
releaseSwapchainLockedWith afterContextIdle (LockedSwapchain swapchain) = mask_ $ do
  initial <- readMVar (swapchainState swapchain)
  if not afterContextIdle && isPoisoned initial
    then pure ()
    else do
      (owned, forceDrain) <-
        modifyMVarMasked (swapchainState swapchain) $ \state ->
          let (retiring, value) = releaseCoreState state
           in pure (retiring, (value, afterContextIdle && isPoisoned state))
      traverse_ (releaseRuntime forceDrain) owned
      modifyMVarMasked (swapchainState swapchain) (const (pure (CoreReleased, ())))
 where
  isPoisoned state = case state of
    CorePoisoned _ -> True
    _ -> False
  releaseRuntime forceDrain runtime = do
    let generations = maybe id (:) (runtimeGeneration runtime) (runtimeRetiredGenerations runtime)
        partial = runtimePartialGeneration runtime
    retirement <-
      trySynchronous $
        if forceDrain
          then forceDrainSlots (swapchainSlots swapchain)
          else retireSlots (swapchainContext swapchain) (swapchainSlots swapchain)
    deviceLost <- case retirement of
      Right () -> pure False
      Left error'
        | isDeviceLostException error' -> do
            forceDrainSlots (swapchainSlots swapchain)
            pure True
        | otherwise -> throwIO error'
    let closeGenerationGate =
          if afterContextIdle
            then Lifetime.sealLifetimeGate
            else Lifetime.closeLifetimeGate
    traverse_ (closeGenerationGate . generationLifetime) generations
    traverse_ (closeGenerationGate . rawGenerationLifetime) partial
    unless (deviceLost || forceDrain) $ do
      presentIdle <- trySynchronous (idlePresentQueue (swapchainSurface swapchain))
      case presentIdle of
        Right () -> pure ()
        Left error'
          | isDeviceLostException error' -> pure ()
          | otherwise -> throwIO error'
    cleanupResults <-
      sequence
        [ trySynchronous (cleanupGenerationOwners (map generationOwner generations))
        , trySynchronous (traverse_ (cleanupGenerationOwner . rawGenerationOwner) partial)
        , trySynchronous (destroyFrameSlots (swapchainContext swapchain) (swapchainSlots swapchain))
        ]
    throwCleanupFailures cleanupResults

cleanupRetiredGenerations :: Swapchain -> IO ()
cleanupRetiredGenerations swapchain = do
  state <- readMVar (swapchainState swapchain)
  runtime <- either throwIO pure (activeCoreState state)
  let retired = runtimeRetiredGenerations runtime
  unless (null retired) $ do
    retireSlots (swapchainContext swapchain) (swapchainSlots swapchain)
    traverse_ (Lifetime.closeLifetimeGate . generationLifetime) retired
    idlePresentQueue (swapchainSurface swapchain)
    results <- forM retired (trySynchronous . cleanupGenerationOwner . generationOwner)
    pending <- forM retired (generationOwnerPending . generationOwner)
    modifyMVarMasked (swapchainState swapchain) $ \current -> do
      active <- either throwIO pure (activeCoreState current)
      let remaining = [generation | (generation, True) <- zip retired pending]
      pure (CoreActive active{runtimeRetiredGenerations = remaining}, ())
    throwCleanupFailures results

cleanupGenerationOwners :: [GenerationOwner] -> IO ()
cleanupGenerationOwners owners = do
  results <- traverse (trySynchronous . cleanupGenerationOwner) owners
  throwCleanupFailures results

forceDrainSlots :: Vector FrameSlot -> IO ()
forceDrainSlots slots = do
  results <- traverse (trySynchronous . forceDrainSlotState . frameSlotState) (Vector.toList slots)
  throwCleanupFailures results

cleanupPartialGeneration :: Swapchain -> IO ()
cleanupPartialGeneration swapchain = do
  state <- readMVar (swapchainState swapchain)
  runtime <- either throwIO pure (activeCoreState state)
  traverse_ cleanupPartial (runtimePartialGeneration runtime)
 where
  cleanupPartial raw = do
    Lifetime.closeLifetimeGate (rawGenerationLifetime raw)
    cleanupGenerationOwner (rawGenerationOwner raw)
    modifyMVarMasked (swapchainState swapchain) $ \state -> do
      runtime <- either throwIO pure (activeCoreState state)
      let remaining = case runtimePartialGeneration runtime of
            Just current
              | rawGenerationHandle current == rawGenerationHandle raw -> Nothing
            current -> current
      pure (CoreActive runtime{runtimePartialGeneration = remaining}, ())

preflightGeneration :: Swapchain -> RuntimeState -> IO GenerationPreflight
preflightGeneration swapchain runtime = do
  let context = swapchainContext swapchain
      surface = swapchainSurface swapchain
      config = swapchainConfig swapchain
      number = runtimeNextGeneration runtime
      oldSwapchain = maybe zero generationHandle (runtimeGeneration runtime)
      physicalDevice = contextPhysicalDevice context
      rawSurface = surfaceHandle surface
  capabilities <- mapVulkan "vkGetPhysicalDeviceSurfaceCapabilitiesKHR" (Surface.getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice rawSurface)
  formats <- enumerateComplete "vkGetPhysicalDeviceSurfaceFormatsKHR" (Surface.getPhysicalDeviceSurfaceFormatsKHR physicalDevice rawSurface)
  presentModes <- enumerateComplete "vkGetPhysicalDeviceSurfacePresentModesKHR" (Surface.getPhysicalDeviceSurfacePresentModesKHR physicalDevice rawSurface)
  surfaceFormat <- either throwIO pure (chooseSurfaceFormat (Vector.toList formats))
  unless (Surface.supportedUsageFlags capabilities .&. Usage.IMAGE_USAGE_COLOR_ATTACHMENT_BIT /= zero) $
    throwIO (SwapchainUsageUnsupported (show (Surface.supportedUsageFlags capabilities)))
  providerExtent <-
    if flexibleExtent (Surface.currentExtent capabilities)
      then sequenceA (surfaceFramebufferExtent surface)
      else pure Nothing
  extentChoice <-
    either throwIO pure $
      chooseExtent
        (Surface.currentExtent capabilities)
        (Surface.minImageExtent capabilities)
        (Surface.maxImageExtent capabilities)
        providerExtent
  case extentChoice of
    ExtentMinimized -> pure (GenerationMinimized number)
    ExtentReady extent -> do
      let graphicsFamily = queueFamilyIndex (graphicsQueue context)
          presentFamily = queueFamilyIndex (surfacePresentQueue surface)
          (sharingMode, queueFamilies) = sharingParameters graphicsFamily presentFamily
          createInfo =
            KHR.SwapchainCreateInfoKHR
              { KHR.next = ()
              , KHR.flags = zero
              , KHR.surface = rawSurface
              , KHR.minImageCount = chooseImageCount (Surface.minImageCount capabilities) (Surface.maxImageCount capabilities)
              , KHR.imageFormat = Surface.format surfaceFormat
              , KHR.imageColorSpace = Surface.colorSpace surfaceFormat
              , KHR.imageExtent = extent
              , KHR.imageArrayLayers = 1
              , KHR.imageUsage = Usage.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
              , KHR.imageSharingMode = sharingMode
              , KHR.queueFamilyIndices = queueFamilies
              , KHR.preTransform = Surface.currentTransform capabilities
              , KHR.compositeAlpha = chooseCompositeAlpha (Surface.supportedCompositeAlpha capabilities)
              , KHR.presentMode = choosePresentMode (presentModePreference config) (Vector.toList presentModes)
              , KHR.clipped = True
              , KHR.oldSwapchain = oldSwapchain
              }
      pure (GenerationReady (GenerationCreatePlan number extent createInfo))

createRawGeneration :: Context -> GenerationCreatePlan -> IO RawGeneration
createRawGeneration context plan = do
  let device = contextDevice context
      createInfo = generationPlanCreateInfo plan
  handle <- mapVulkan "vkCreateSwapchainKHR" (KHR.createSwapchainKHR device createInfo Nothing)
  let destroy = KHR.destroySwapchainKHR device handle Nothing
  ( do
      setObjectNameLeased context ObjectType.OBJECT_TYPE_SWAPCHAIN_KHR (swapchainHandleWord handle) (derivedObjectName "swapchain" (swapchainHandleWord handle))
      gate <- Lifetime.newLifetimeGate
      rawCleanup <- newCleanupAction ("swapchain generation " <> show (generationPlanNumber plan) <> " raw swapchain") destroy
      owner <- GenerationOwner <$> newMVar [rawCleanup]
      pure (RawGeneration (generationPlanNumber plan) handle (generationPlanExtent plan) gate owner)
    )
    `onException` destroy

buildRawGeneration :: Context -> RawGeneration -> IO Generation
buildRawGeneration context raw = do
  rawImages <- enumerateComplete "vkGetSwapchainImagesKHR" (KHR.getSwapchainImagesKHR (contextDevice context) (rawGenerationHandle raw))
  when (Vector.null rawImages) (throwIO (VulkanFailure "vkGetSwapchainImagesKHR" "the swapchain returned no images"))
  images <- createGenerationImages context (rawGenerationLifetime raw) (rawGenerationOwner raw) (rawGenerationExtent raw) (Vector.toList rawImages)
  pure
    ( Generation
        (rawGenerationNumber raw)
        (rawGenerationHandle raw)
        (extentTuple (rawGenerationExtent raw))
        (rawGenerationLifetime raw)
        (Vector.fromList images)
        (rawGenerationOwner raw)
    )

createGenerationImages :: Context -> LifetimeGate -> GenerationOwner -> Fundamental.Extent2D -> [Vk.Image] -> IO [GenerationImage]
createGenerationImages context gate owner extent = go 0
 where
  go _ [] = pure []
  go index (rawImage : remaining) = do
    image <- createGenerationImage context gate owner extent index rawImage
    rest <- go (index + 1) remaining
    pure (image : rest)

createGenerationImage :: Context -> LifetimeGate -> GenerationOwner -> Fundamental.Extent2D -> Word32 -> Vk.Image -> IO GenerationImage
createGenerationImage context gate owner extent index rawImage = do
  state <- newImageState 1 1
  generation <- Lifetime.newResourceGeneration
  let range = ImageView.ImageSubresourceRange Aspect.IMAGE_ASPECT_COLOR_BIT 0 1 0 1
      viewInfo =
        (zero :: ImageView.ImageViewCreateInfo '[])
          { ImageView.image = rawImage
          , ImageView.viewType = ViewType.IMAGE_VIEW_TYPE_2D
          , ImageView.format = Format.FORMAT_B8G8R8A8_SRGB
          , ImageView.subresourceRange = range
          }
      device = contextDevice context
  view <- mapVulkan "vkCreateImageView(swapchain)" (ImageView.createImageView device viewInfo Nothing)
  setObjectNameLeased context ObjectType.OBJECT_TYPE_IMAGE_VIEW (imageViewWord view) (derivedObjectName "image-view-swapchain" (imageViewWord view))
    `onException` ImageView.destroyImageView device view Nothing
  viewCleanup <- registerGenerationCleanup owner ("swapchain image " <> show index <> " view") (ImageView.destroyImageView device view Nothing)
  renderFinished <- mapVulkan "vkCreateSemaphore(swapchain render-finished)" (Semaphore.createSemaphore device (Semaphore.SemaphoreCreateInfo () zero) Nothing)
  setObjectNameLeased context ObjectType.OBJECT_TYPE_SEMAPHORE (semaphoreHandleWord renderFinished) (derivedObjectName "semaphore-swapchain-render-finished" (semaphoreHandleWord renderFinished))
    `onException` Semaphore.destroySemaphore device renderFinished Nothing
  renderFinishedCleanup <- registerGenerationCleanup owner ("swapchain image " <> show index <> " render-finished semaphore") (Semaphore.destroySemaphore device renderFinished Nothing)
  renderState <- newMVar RenderFinishedIdle
  let acquire = do
        lease <- Lifetime.acquireLifetimeLease gate
        maybe (throwIO FrameExpired) pure lease
      quarantine = do
        quarantineImageState state
        Lifetime.quarantineLifetimeGate gate
      target = Pipeline.ColorImage (managedImageRuntimeHandleWithQuarantine (contextIdentity context) generation acquire quarantine (imageMetadata rawImage view state extent))
  pure (GenerationImage index generation rawImage view state target renderFinished renderState viewCleanup renderFinishedCleanup)

imageMetadata :: Vk.Image -> Vk.ImageView -> ImageState -> Fundamental.Extent2D -> ImageBindingMetadata
imageMetadata rawImage view state extent =
  ImageBindingMetadata
    { imageBindingRawHandle = rawImage
    , imageBindingRawView = view
    , imageBindingState = state
    , imageBindingExtent = Fundamental.Extent3D (extentWidth extent) (extentHeight extent) 1
    , imageBindingFormat = Format.FORMAT_B8G8R8A8_SRGB
    , imageBindingAspect = Aspect.IMAGE_ASPECT_COLOR_BIT
    , imageBindingSamples = Samples.SAMPLE_COUNT_1_BIT
    , imageBindingMipLevel = 0
    , imageBindingArrayLayer = 0
    , imageBindingMipLevels = 1
    , imageBindingArrayLayers = 1
    , imageBindingUsage = Usage.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
    }

imageViewWord :: Vk.ImageView -> Word64
imageViewWord (Vk.ImageView handle) = handle

swapchainHandleWord :: Extensions.SwapchainKHR -> Word64
swapchainHandleWord (Extensions.SwapchainKHR handle) = handle

commandPoolHandleWord :: Vk.CommandPool -> Word64
commandPoolHandleWord (Vk.CommandPool handle) = handle

semaphoreHandleWord :: Vk.Semaphore -> Word64
semaphoreHandleWord (Vk.Semaphore handle) = handle

commandBufferHandleWord :: Vk.CommandBuffer -> Word64
commandBufferHandleWord = fromIntegral . ptrToWordPtr . Vk.commandBufferHandle

registerGenerationCleanup :: GenerationOwner -> String -> IO () -> IO CleanupAction
registerGenerationCleanup owner label cleanup = do
  action <- newCleanupAction label cleanup
  modifyMVarMasked (generationOwnerCleanup owner) (\actions -> pure (action : actions, ()))
  pure action

cleanupGenerationOwner :: GenerationOwner -> IO ()
cleanupGenerationOwner owner = do
  actions <- readMVar (generationOwnerCleanup owner)
  runCleanupActions actions
  modifyMVarMasked (generationOwnerCleanup owner) (const (pure ([], ())))

generationOwnerPending :: GenerationOwner -> IO Bool
generationOwnerPending owner = do
  actions <- readMVar (generationOwnerCleanup owner)
  or <$> traverse cleanupActionPending actions

newCleanupAction :: String -> IO () -> IO CleanupAction
newCleanupAction label cleanup = CleanupAction label <$> newMVar (Just cleanup)

newCleanupActions :: String -> [IO ()] -> IO [CleanupAction]
newCleanupActions label actions =
  traverse (uncurry newCleanupAction) (zipWith (\index action -> (label <> " " <> show index, action)) [(0 :: Int) ..] actions)

cleanupActionPending :: CleanupAction -> IO Bool
cleanupActionPending action = isJust <$> readMVar (cleanupActionState action)

runCleanupAction :: CleanupAction -> IO (Maybe String)
runCleanupAction action =
  modifyMVarMasked (cleanupActionState action) $ \case
    Nothing -> pure (Nothing, Nothing)
    Just cleanup -> do
      result <- trySynchronous cleanup
      case result of
        Right () -> pure (Nothing, Nothing)
        Left error' -> pure (Just cleanup, Just (cleanupActionLabel action <> ": " <> show error'))

runCleanupActions :: [CleanupAction] -> IO ()
runCleanupActions actions = do
  failures <- foldr (\failure rest -> maybe rest (: rest) failure) [] <$> traverse runCleanupAction actions
  unless (null failures) (throwIO (CleanupFailed failures))

newCleanupActionsForTest :: [IO ()] -> IO [CleanupAction]
newCleanupActionsForTest = newCleanupActions "test cleanup action"

runCleanupActionsForTest :: [CleanupAction] -> IO ()
runCleanupActionsForTest = runCleanupActions

pendingCleanupActionsForTest :: [CleanupAction] -> IO Int
pendingCleanupActionsForTest actions = length . filter id <$> traverse cleanupActionPending actions

lookupOrCreateSlotValueForTest :: (Eq key) => MVar [(key, value)] -> key -> IO value -> IO value
lookupOrCreateSlotValueForTest storage key create =
  modifyMVarMasked storage $ \entries ->
    case lookup key entries of
      Just value -> pure (entries, value)
      Nothing -> do
        value <- create
        pure (entries <> [(key, value)], value)

resetSlotValuesForTest :: MVar [(key, value)] -> (value -> IO ()) -> IO ()
resetSlotValuesForTest storage reset = readMVar storage >>= traverse_ (reset . snd)

createFrameSlots :: Context -> Int -> IO (Vector FrameSlot)
createFrameSlots context count = Vector.fromList <$> go 0
 where
  go index
    | index >= count = pure []
    | otherwise = do
        slot <- createFrameSlot context index
        rest <- go (index + 1) `onException` destroyFrameSlot context slot
        pure (slot : rest)

createFrameSlot :: Context -> Int -> IO FrameSlot
createFrameSlot context index = do
  let device = contextDevice context
      poolInfo =
        (zero :: CommandPool.CommandPoolCreateInfo)
          { CommandPool.flags = PoolFlags.COMMAND_POOL_CREATE_TRANSIENT_BIT .|. PoolFlags.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
          , CommandPool.queueFamilyIndex = queueFamilyIndex (graphicsQueue context)
          }
  imageAvailable <- mapVulkan "vkCreateSemaphore(swapchain image-available)" (Semaphore.createSemaphore device (Semaphore.SemaphoreCreateInfo () zero) Nothing)
  setObjectNameLeased context ObjectType.OBJECT_TYPE_SEMAPHORE (semaphoreHandleWord imageAvailable) (derivedObjectName "semaphore-swapchain-acquire" (semaphoreHandleWord imageAvailable))
    `onException` Semaphore.destroySemaphore device imageAvailable Nothing
  pool <- mapVulkan "vkCreateCommandPool(swapchain slot)" (CommandPool.createCommandPool device poolInfo Nothing) `onException` Semaphore.destroySemaphore device imageAvailable Nothing
  setObjectNameLeased context ObjectType.OBJECT_TYPE_COMMAND_POOL (commandPoolHandleWord pool) (derivedObjectName "command-pool-swapchain-slot" (commandPoolHandleWord pool))
    `onException` (CommandPool.destroyCommandPool device pool Nothing >> Semaphore.destroySemaphore device imageAvailable Nothing)
  buffers <-
    mapVulkan "vkAllocateCommandBuffers(swapchain slot)" (CommandBuffer.allocateCommandBuffers device (CommandBuffer.CommandBufferAllocateInfo pool CommandLevel.COMMAND_BUFFER_LEVEL_PRIMARY 1))
      `onException` (CommandPool.destroyCommandPool device pool Nothing >> Semaphore.destroySemaphore device imageAvailable Nothing)
  commandBuffer <- case Vector.toList buffers of
    [buffer] -> pure buffer
    _ -> do
      CommandPool.destroyCommandPool device pool Nothing
      Semaphore.destroySemaphore device imageAvailable Nothing
      throwIO (VulkanFailure "vkAllocateCommandBuffers(swapchain slot)" "expected exactly one primary command buffer")
  setObjectNameLeased context ObjectType.OBJECT_TYPE_COMMAND_BUFFER (commandBufferHandleWord commandBuffer) (derivedObjectName "command-buffer-swapchain-slot" (commandBufferHandleWord commandBuffer))
    `onException` (CommandPool.destroyCommandPool device pool Nothing >> Semaphore.destroySemaphore device imageAvailable Nothing)
  state <- newMVar SlotIdle
  poolCleanup <- newCleanupAction ("swapchain slot " <> show index <> " command pool") (CommandPool.destroyCommandPool device pool Nothing)
  imageAvailableCleanup <- newCleanupAction ("swapchain slot " <> show index <> " acquire semaphore") (Semaphore.destroySemaphore device imageAvailable Nothing)
  descriptorStorage <- newMVar []
  pure (FrameSlot index imageAvailable pool commandBuffer state poolCleanup imageAvailableCleanup descriptorStorage)

retireSlots :: Context -> Vector FrameSlot -> IO ()
retireSlots context slots = do
  results <- traverse (trySynchronous . retireFrameSlot context) (Vector.toList slots)
  case [error' | Left error' <- results, isDeviceLostException error'] of
    _ : _ -> throwIO DeviceLost
    [] -> throwCleanupFailures results

retireFrameSlot :: Context -> FrameSlot -> IO ()
retireFrameSlot context slot =
  retireSlotState
    (waitTimelineLeased (graphicsQueue context))
    (resetFrameSlotStorage context slot)
    (frameSlotState slot)

retireSlotState :: (Word64 -> IO ()) -> IO () -> MVar SlotState -> IO ()
retireSlotState waitTimeline afterRetirement stateVariable =
  modifyMVarMasked stateVariable $ \case
    SlotIdle -> afterRetirement >> pure (SlotIdle, ())
    SlotAcquired ownership ->
      throwIO
        ( VulkanFailure
            "retire swapchain slot"
            ("acquire semaphore is outstanding for " <> show ownership <> "; publish an explicit recovery submission first")
        )
    SlotSubmitted _ timeline releases -> do
      waitTimeline timeline
      runCleanupActions releases
      afterRetirement
      pure (SlotIdle, ())

forceDrainSlotState :: MVar SlotState -> IO ()
forceDrainSlotState stateVariable =
  modifyMVarMasked stateVariable $ \case
    SlotIdle -> pure (SlotIdle, ())
    SlotAcquired _ -> pure (SlotIdle, ())
    SlotSubmitted _ _ releases -> do
      runCleanupActions releases
      pure (SlotIdle, ())

newSubmittedSlotStateForTest :: FrameOwnership -> Word64 -> [IO ()] -> IO (MVar SlotState)
newSubmittedSlotStateForTest ownership timeline releases = do
  actions <- newCleanupActions "test retained release" releases
  newMVar (SlotSubmitted ownership timeline actions)

newIdleSlotStateForTest :: IO (MVar SlotState)
newIdleSlotStateForTest = newMVar SlotIdle

retainAcquiredSlotForTest :: MVar SlotState -> FrameOwnership -> IO ()
retainAcquiredSlotForTest = retainAcquiredState

retireSlotStateForTest :: MVar SlotState -> (Word64 -> IO ()) -> IO () -> IO ()
retireSlotStateForTest stateVariable waitTimeline afterRetirement = retireSlotState waitTimeline afterRetirement stateVariable

forceDrainSlotStateForTest :: MVar SlotState -> IO ()
forceDrainSlotStateForTest = forceDrainSlotState

inspectSlotStateForTest :: MVar SlotState -> IO SlotStateView
inspectSlotStateForTest stateVariable = slotStateView <$> readMVar stateVariable

{- | Prepare stable slot storage for a new frame. Waiting and retained-resource
release always precede command-pool reset.
-}
prepareFrameSlot :: LockedSwapchain -> FrameSlot -> IO ()
prepareFrameSlot (LockedSwapchain swapchain) slot = do
  let context = swapchainContext swapchain
  retireSlotState
    (waitTimelineLeased (graphicsQueue context))
    (resetFrameSlotStorage context slot)
    (frameSlotState slot)

resetFrameSlotStorage :: Context -> FrameSlot -> IO ()
resetFrameSlotStorage context slot = do
  resetSlotValuesForTest (frameSlotDescriptorStorage slot) resetDescriptorFrameLeased
  mapVulkan
    "vkResetCommandPool(swapchain slot)"
    (CommandPool.resetCommandPool (contextDevice context) (frameSlotCommandPool slot) zero)

destroyFrameSlots :: Context -> Vector FrameSlot -> IO ()
destroyFrameSlots context slots = do
  results <- traverse (trySynchronous . destroyFrameSlot context) (reverse (Vector.toList slots))
  throwCleanupFailures results

destroyFrameSlot :: Context -> FrameSlot -> IO ()
destroyFrameSlot _ slot = do
  state <- slotStateView <$> readMVar (frameSlotState slot)
  unless (state == SlotStateIdle) $
    throwIO (VulkanFailure "destroy swapchain slot" ("slot is not safely retired: " <> show state))
  descriptorFrames <- fmap snd <$> readMVar (frameSlotDescriptorStorage slot)
  traverse_ destroyDescriptorFrameLeased descriptorFrames
  modifyMVarMasked (frameSlotDescriptorStorage slot) (const (pure ([], ())))
  runCleanupActions [frameSlotPoolCleanup slot, frameSlotImageAvailableCleanup slot]

idlePresentQueue :: Surface -> IO ()
idlePresentQueue surface =
  withQueueHandleLockedLeased (surfacePresentQueue surface) $ \queue ->
    mapVulkan "vkQueueWaitIdle(present)" (VkQueue.queueWaitIdle queue)

validateFramesInFlight :: Int -> Either VpipeError ()
validateFramesInFlight count
  | count > 0 = Right ()
  | otherwise = Left (InvalidFramesInFlight count)

chooseSurfaceFormat :: [Surface.SurfaceFormatKHR] -> Either VpipeError Surface.SurfaceFormatKHR
chooseSurfaceFormat formats =
  case filter required formats of
    format : _ -> Right format
    [] -> Left (SwapchainFormatUnavailable (map show formats))
 where
  required format = Surface.format format == Format.FORMAT_B8G8R8A8_SRGB && Surface.colorSpace format == Surface.COLOR_SPACE_SRGB_NONLINEAR_KHR

chooseImageCount :: Word32 -> Word32 -> Word32
chooseImageCount minimumCount maximumCount
  | maximumCount == 0 = incrementSaturated minimumCount
  | otherwise = min maximumCount (incrementSaturated minimumCount)

incrementSaturated :: Word32 -> Word32
incrementSaturated value
  | value == maxBound = maxBound
  | otherwise = value + 1

choosePresentMode :: PresentMode -> [Surface.PresentModeKHR] -> Surface.PresentModeKHR
choosePresentMode preference available
  | preferred `elem` available = preferred
  | otherwise = Surface.PRESENT_MODE_FIFO_KHR
 where
  preferred = case preference of
    Fifo -> Surface.PRESENT_MODE_FIFO_KHR
    Mailbox -> Surface.PRESENT_MODE_MAILBOX_KHR
    Immediate -> Surface.PRESENT_MODE_IMMEDIATE_KHR

data ExtentChoice = ExtentMinimized | ExtentReady Fundamental.Extent2D
  deriving stock (Eq, Show)

chooseExtent :: Fundamental.Extent2D -> Fundamental.Extent2D -> Fundamental.Extent2D -> Maybe (Int, Int) -> Either VpipeError ExtentChoice
chooseExtent current minimumExtent maximumExtent provider
  | not (flexibleExtent current) = classify current
  | otherwise = case provider of
      Nothing -> Left SwapchainExtentUnavailable
      Just (width, height)
        | width <= 0 || height <= 0 -> Right ExtentMinimized
        | otherwise -> Right (ExtentReady (Fundamental.Extent2D (clampDimension width (extentWidth minimumExtent) (extentWidth maximumExtent)) (clampDimension height (extentHeight minimumExtent) (extentHeight maximumExtent))))
 where
  classify extent
    | extentWidth extent == 0 || extentHeight extent == 0 = Right ExtentMinimized
    | otherwise = Right (ExtentReady extent)

flexibleExtent :: Fundamental.Extent2D -> Bool
flexibleExtent extent = extentWidth extent == maxBound && extentHeight extent == maxBound

extentWidth :: Fundamental.Extent2D -> Word32
extentWidth (Fundamental.Extent2D width _) = width

extentHeight :: Fundamental.Extent2D -> Word32
extentHeight (Fundamental.Extent2D _ height) = height

clampDimension :: Int -> Word32 -> Word32 -> Word32
clampDimension value minimumValue maximumValue = max minimumValue (min maximumValue converted)
 where
  converted = fromInteger (min (toInteger (maxBound :: Word32)) (toInteger value))

data FamilySharing = ExclusiveFamily Word32 | ConcurrentFamilies Word32 Word32
  deriving stock (Eq, Show)

chooseFamilySharing :: Word32 -> Word32 -> FamilySharing
chooseFamilySharing graphics present
  | graphics == present = ExclusiveFamily graphics
  | otherwise = ConcurrentFamilies graphics present

sharingParameters :: Word32 -> Word32 -> (Sharing.SharingMode, Vector Word32)
sharingParameters graphics present = case chooseFamilySharing graphics present of
  ExclusiveFamily _ -> (Sharing.SHARING_MODE_EXCLUSIVE, Vector.empty)
  ConcurrentFamilies first second -> (Sharing.SHARING_MODE_CONCURRENT, Vector.fromList [first, second])

chooseCompositeAlpha :: Surface.CompositeAlphaFlagsKHR -> Surface.CompositeAlphaFlagBitsKHR
chooseCompositeAlpha supported
  | contains Surface.COMPOSITE_ALPHA_OPAQUE_BIT_KHR = Surface.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
  | contains Surface.COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = Surface.COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
  | contains Surface.COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = Surface.COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
  | otherwise = Surface.COMPOSITE_ALPHA_INHERIT_BIT_KHR
 where
  contains bit = supported .&. bit /= zero

extentTuple :: Fundamental.Extent2D -> (Word32, Word32)
extentTuple extent = (extentWidth extent, extentHeight extent)

trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous action = do
  result <- try action
  case result of
    Left error' -> case fromException error' :: Maybe AsyncException of
      Just _ -> throwIO error'
      Nothing -> pure (Left error')
    Right value -> pure (Right value)

isDeviceLostException :: SomeException -> Bool
isDeviceLostException error' = case fromException error' :: Maybe VpipeError of
  Just DeviceLost -> True
  _ -> False

throwCleanupFailures :: [Either SomeException ()] -> IO ()
throwCleanupFailures results =
  case [show error' | Left error' <- results] of
    [] -> pure ()
    failures -> throwIO (CleanupFailed failures)

mapVulkan :: String -> IO a -> IO a
mapVulkan operation action =
  action `catchVulkan` \result -> case result of
    Result.ERROR_DEVICE_LOST -> throwIO DeviceLost
    Result.ERROR_SURFACE_LOST_KHR -> throwIO SurfaceLost
    _ -> throwIO (VulkanFailure operation (show result))

enumerateComplete :: String -> IO (Result.Result, Vector a) -> IO (Vector a)
enumerateComplete operation action = do
  (result, values) <- mapVulkan operation action
  case result of
    Result.SUCCESS -> pure values
    Result.INCOMPLETE -> enumerateComplete operation action
    _ -> throwIO (VulkanFailure operation (show result))

enumerateCompleteForTest :: String -> IO (Result.Result, Vector a) -> IO (Vector a)
enumerateCompleteForTest = enumerateComplete

catchVulkan :: IO a -> (Result.Result -> IO a) -> IO a
catchVulkan action handler =
  action `catch` \(error' :: Vulkan.VulkanException) -> handler (Vulkan.vulkanExceptionResult error')
