{-# LANGUAGE ScopedTypeVariables #-}

module Vpipe.GraphicsSubmissionTest (graphicsSubmissionTests) where

import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (AsyncException (ThreadKilled), fromException, throwIO)
import Data.Maybe (fromMaybe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import Vpipe.Error (VpipeError (VulkanFailure))
import Vpipe.Graphics.Submission.Internal

graphicsSubmissionTests :: TestTree
graphicsSubmissionTests =
  testGroup
    "graphics submission ownership"
    [ testCase "normal completion needs no fallback" normalCompletionTest
    , testCase "a successful fallback preserves the primary failure" recoveredCompletionTest
    , testCase "two failures report uncertain completion" uncertainCompletionTest
    , testCase "transferred cleanup is not released by its former owner" transferredOwnershipTest
    , testCase "retirement registers before transferring cleanup" registeredRetirementTest
    , testCase "registration failure abandons uncertain cleanup locally" failedRegistrationTest
    ]

normalCompletionTest :: IO ()
normalCompletionTest = do
  fallbackRuns <- newMVar (0 :: Int)
  status <- confirmSubmittedWork (pure ()) (modifyMVar_ fallbackRuns (pure . (+ 1)))
  case status of
    SubmittedWorkComplete -> pure ()
    _ -> assertFailure "expected confirmed completion"
  readMVar fallbackRuns >>= (@?= 0)

recoveredCompletionTest :: IO ()
recoveredCompletionTest = do
  status <- confirmSubmittedWork (throwIO ThreadKilled) (pure ())
  case status of
    SubmittedWorkCompleteAfterFailure primary ->
      fromException primary @?= Just ThreadKilled
    _ -> assertFailure "expected the fallback to confirm completion"

uncertainCompletionTest :: IO ()
uncertainCompletionTest = do
  let primary = VulkanFailure "primary" "failed"
      fallback = VulkanFailure "fallback" "failed"
  status <- confirmSubmittedWork (throwIO primary) (throwIO fallback)
  case status of
    SubmittedWorkUncertain primaryFailure fallbackFailure -> do
      fromException primaryFailure @?= Just primary
      fromException fallbackFailure @?= Just fallback
    _ -> assertFailure "expected uncertain completion"

transferredOwnershipTest :: IO ()
transferredOwnershipTest = do
  events <- newMVar ([] :: [String])
  let record value = modifyMVar_ events (pure . (<> [value]))
  owned <- newOwnedActions [record "pool", record "leases"]
  transferred <- transferOwnedActions owned
  releaseOwnedActions owned
  readMVar events >>= (@?= [])
  case transferred of
    Nothing -> assertFailure "expected cleanup ownership to transfer"
    Just actions -> releaseActions actions
  readMVar events >>= (@?= ["pool", "leases"])

registeredRetirementTest :: IO ()
registeredRetirementTest = do
  events <- newMVar ([] :: [String])
  registered <- newMVar Nothing
  let record value = modifyMVar_ events (pure . (<> [value]))
      register finalizer = modifyMVar_ registered (const (pure (Just finalizer)))
  pool <- newOwnedActions [record "pool"]
  leases <- newOwnedActions [record "leases"]
  result <- retireOwnedActions register [pool, leases]
  case result of
    Left failure -> assertFailure ("retirement registration failed: " <> show failure)
    Right () -> pure ()
  releaseOwnedActions pool
  releaseOwnedActions leases
  readMVar events >>= (@?= [])
  readMVar registered >>= fromMaybe (assertFailure "no retirement finalizer was registered")
  readMVar events >>= (@?= ["pool", "leases"])

failedRegistrationTest :: IO ()
failedRegistrationTest = do
  events <- newMVar ([] :: [String])
  let registrationFailure = VulkanFailure "register retirement" "failed"
      record value = modifyMVar_ events (pure . (<> [value]))
  pool <- newOwnedActions [record "pool"]
  leases <- newOwnedActions [record "leases"]
  result <- retireOwnedActions (const (throwIO registrationFailure)) [pool, leases]
  case result of
    Left failure -> fromException failure @?= Just registrationFailure
    Right () -> assertFailure "expected retirement registration to fail"
  releaseOwnedActions pool
  releaseOwnedActions leases
  readMVar events >>= (@?= [])
