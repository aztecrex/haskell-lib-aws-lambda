module Spec.Cloud.Compute.Ephemeral (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = testGroup "Ephemeral" [
        testCase "display something" $ pure ()
    ]
