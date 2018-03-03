module Spec.Cloud.AWS.Lambda (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = testGroup "Lambda" [
        testCase "just display something" $ pure ()
    ]
