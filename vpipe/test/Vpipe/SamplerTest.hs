{-# LANGUAGE DataKinds #-}

module Vpipe.SamplerTest (samplerTests) where

import Control.Exception (throwIO, try)
import Control.Monad (void)
import Data.List (isInfixOf)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Vpipe.Context (Context, VpipeConfig (vpipeValidationStrict), contextMaxSamplerLodBias, defaultVpipeConfig, withVpipe)
import Vpipe.Error (VpipeError (..))
import Vpipe.Format (Format (D32Sfloat))
import Vpipe.Image (Image, imageExtent2D, newImage)
import Vpipe.Image.Types (Dim (D2), ImageUsage (Sampled))
import Vpipe.Pipeline (ComparisonTextureBinding, TypedTextureBinding, comparisonTextureBinding, typedTextureBinding)
import Vpipe.Sampler (CompareOp (Less), Filter (Nearest), Sampler, defaultSamplerDescription, newSampler, samplerAnisotropy, samplerCompareOp, samplerDescription, samplerMagFilter, samplerMipLodBias)
import Vpipe.Sampler.Internal (acquireSamplerBindingLease, samplerGeneration)

samplerTests :: TestTree
samplerTests =
  testGroup
    "sampler"
    [ testCase "equivalent descriptions share one sampler" deduplicationTest
    , testCase "sampler cache hits share one generation and lifetime" cacheLifetimeIdentityTest
    , testCase "changed descriptions create distinct samplers" distinctDescriptionTest
    , testCase "samplers and images from distinct contexts cannot be bound together" wrongContextBindingTest
    , testCase "invalid anisotropy is rejected before Vulkan creation" invalidAnisotropyTest
    , testCase "mip LOD bias observes the physical-device limit" lodBiasLimitTest
    , testCase "comparison samplers require the comparison texture seam" comparisonBindingTest
    , testCase "retained contexts reject sampler creation after close" retainedContextClosureTest
    , testCase "retained sampler leases reject after Context cleanup" retainedSamplerLeaseTest
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

deduplicationTest :: IO ()
deduplicationTest = withTestContext $ \context -> do
  first <- newSampler context defaultSamplerDescription
  second <- newSampler context defaultSamplerDescription
  assertBool "equivalent descriptions must share sampler identity" (first == second)
  samplerDescription first @?= defaultSamplerDescription

cacheLifetimeIdentityTest :: IO ()
cacheLifetimeIdentityTest = withTestContext $ \context -> do
  first <- newSampler context defaultSamplerDescription
  second <- newSampler context defaultSamplerDescription
  assertBool "cache hits must preserve sampler generation" (samplerGeneration first == samplerGeneration second)
  firstRelease <- acquireSamplerBindingLease first
  secondRelease <- acquireSamplerBindingLease second
  firstRelease
  secondRelease

distinctDescriptionTest :: IO ()
distinctDescriptionTest = withTestContext $ \context -> do
  first <- newSampler context defaultSamplerDescription
  second <- newSampler context defaultSamplerDescription{samplerMagFilter = Nearest}
  assertBool "different descriptions must not share sampler identity" (first /= second)

wrongContextBindingTest :: IO ()
wrongContextBindingTest =
  withTestContext $ \imageContext ->
    withTestContext $ \samplerContext -> do
      image <- newImage imageContext (imageExtent2D 1 1) 1 1 :: IO (Image 'D2 'D32Sfloat '[ 'Sampled])
      sampler <- newSampler samplerContext defaultSamplerDescription
      assertBool "equivalent cross-context samplers must remain distinct" =<< do
        localSampler <- newSampler imageContext defaultSamplerDescription
        pure (localSampler /= sampler)
      result <- try (typedTextureBinding image sampler) :: IO (Either VpipeError (TypedTextureBinding 'D2 'D32Sfloat))
      case result of
        Left (VulkanFailure "pipeline texture binding" detail) ->
          assertBool "owner rejection must identify the Context mismatch" ("different contexts" `isInfixOf` detail)
        unexpected -> assertFailure ("expected cross-context texture rejection, got " <> show (void unexpected))

invalidAnisotropyTest :: IO ()
invalidAnisotropyTest = withTestContext $ \context -> do
  let description = defaultSamplerDescription{samplerAnisotropy = Just 0.5}
  result <- try (newSampler context description) :: IO (Either VpipeError Sampler)
  case result of
    Left (VulkanFailure operation detail) -> do
      assertBool "validation must identify the sampler description" (operation == "sampler description")
      assertBool "validation must explain the invalid anisotropy" ("anisotropy" `isInfixOf` detail)
    unexpected -> assertFailure ("expected anisotropy validation failure, got " <> show (void unexpected))

lodBiasLimitTest :: IO ()
lodBiasLimitTest = withTestContext $ \context -> do
  let limit = contextMaxSamplerLodBias context
  boundary <- newSampler context defaultSamplerDescription{samplerMipLodBias = limit}
  samplerMipLodBias (samplerDescription boundary) @?= limit
  result <- try (newSampler context defaultSamplerDescription{samplerMipLodBias = limit + 1}) :: IO (Either VpipeError Sampler)
  case result of
    Left (VulkanFailure operation detail) -> do
      operation @?= "sampler description"
      assertBool "validation must identify the mip LOD bias limit" ("mip LOD bias" `isInfixOf` detail && "device limit" `isInfixOf` detail)
    unexpected -> assertFailure ("expected mip LOD bias validation failure, got " <> show (void unexpected))

comparisonBindingTest :: IO ()
comparisonBindingTest = withTestContext $ \context -> do
  image <- newImage context (imageExtent2D 1 1) 1 1 :: IO (Image 'D2 'D32Sfloat '[ 'Sampled])
  regularSampler <- newSampler context defaultSamplerDescription
  comparisonSampler <- newSampler context defaultSamplerDescription{samplerCompareOp = Just Less}

  missingCompare <- try (comparisonTextureBinding image regularSampler) :: IO (Either VpipeError (ComparisonTextureBinding 'D2))
  case missingCompare of
    Left (VulkanFailure "comparison texture binding" _) -> pure ()
    unexpected -> assertFailure ("expected a missing comparison-op failure, got " <> show (void unexpected))

  wrongSeam <- try (typedTextureBinding image comparisonSampler) :: IO (Either VpipeError (TypedTextureBinding 'D2 'D32Sfloat))
  case wrongSeam of
    Left (VulkanFailure "regular typed texture binding" _) -> pure ()
    unexpected -> assertFailure ("expected regular sampling to reject a comparison sampler, got " <> show (void unexpected))

  _ <- comparisonTextureBinding image comparisonSampler
  pure ()

retainedContextClosureTest :: IO ()
retainedContextClosureTest = do
  context <- withTestContext pure
  result <- try (newSampler context defaultSamplerDescription) :: IO (Either VpipeError Sampler)
  case result of
    Left ContextClosed -> pure ()
    unexpected -> assertFailure ("expected ContextClosed, got " <> show (void unexpected))

retainedSamplerLeaseTest :: IO ()
retainedSamplerLeaseTest = do
  sampler <- withTestContext (`newSampler` defaultSamplerDescription)
  result <- try (acquireSamplerBindingLease sampler) :: IO (Either VpipeError (IO ()))
  case result of
    Left ContextClosed -> pure ()
    Left error' -> assertFailure ("expected ContextClosed, got " <> show error')
    Right release -> release >> assertFailure "cleaned-up sampler accepted a new lifetime lease"
