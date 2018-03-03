module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Spec.Cloud.Compute as Compute (tests)
import qualified Spec.Cloud.AWS as AWS (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [
        Compute.tests,
        AWS.tests
    ]

