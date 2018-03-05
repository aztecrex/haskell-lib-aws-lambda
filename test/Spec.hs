module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Spec.Cloud as Cloud (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [
        Cloud.tests
    ]

