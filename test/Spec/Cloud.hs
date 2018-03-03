module Spec.Cloud (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Spec.Cloud.Compute as Compute (tests)
import qualified Spec.Cloud.AWS as AWS (tests)


tests :: TestTree
tests = testGroup "Cloud" [
        Compute.tests
    ]
