module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))

import Cloud.Compute.AWS.Lambda (checkWire)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [
    testCase "all options" $ do
        let actual = checkWire
        actual @?= "checked"
  ]
