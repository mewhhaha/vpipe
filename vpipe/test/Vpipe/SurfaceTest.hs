module Vpipe.SurfaceTest (surfaceTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Vpipe.Context.Device (CandidateDevice (..), choosePresentFamily, queueFamilyUnion)

surfaceTests :: TestTree
surfaceTests =
  testGroup
    "surface"
    [ testCase "queue family union includes ordered present families once" $
        queueFamilyUnion candidate @?= [0, 1, 2, 3]
    , testCase "presentation prefers graphics and otherwise preserves first supported family" $ do
        choosePresentFamily (Just 1) [(0, True), (1, True)] @?= Just 1
        choosePresentFamily (Just 1) [(2, True), (1, False), (3, True)] @?= Just 2
        choosePresentFamily (Just 1) [(1, False)] @?= Nothing
    ]
 where
  candidate =
    CandidateDevice
      { candidateHandle = error "model only"
      , candidateName = "model"
      , candidateDeviceType = error "model only"
      , candidateScore = 0
      , candidateRejection = []
      , candidateGraphicsFamily = Just 0
      , candidateComputeFamily = Just 1
      , candidateTransferFamily = Just 2
      , candidatePresentFamilies = [0, 3, 1, 3]
      , candidateEnabledExtensions = []
      , candidateMaxTimelineDifference = 0
      , candidateSamplerAnisotropy = False
      }
