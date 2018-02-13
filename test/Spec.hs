module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Functor.Identity (Identity(..), runIdentity)

import Cloud.Compute.AWS.Lambda (runLambdaT, liftLambdaT)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [
    testCase "lambda runs" $ do
        let embedded = 17
            lambda = liftLambdaT (Identity embedded)
        runIdentity (runLambdaT lambda "anything") @?= embedded
  ]
