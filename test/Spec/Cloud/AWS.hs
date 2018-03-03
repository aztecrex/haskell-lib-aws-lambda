module Spec.Cloud.AWS (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Spec.Cloud.AWS.Lambda as Lambda (tests)

tests :: TestTree
tests = testGroup "AWS" [
        Lambda.tests
    ]
