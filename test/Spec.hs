module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Functor.Identity (Identity(..), runIdentity)

import Cloud.Compute.AWS.Lambda (runLambdaT, liftLambdaT)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [
    testCase "construct lambdat" $ do
        let embedded = 17
        -- when
            lambda = liftLambdaT (Identity embedded)
        -- then
        runIdentity (runLambdaT lambda "anything") @?= embedded,
    testCase "lambdaT functor" $do
        let embedded = 19
            transform = (+1)
            lambda = liftLambdaT (Identity embedded)
        -- when
            actual = transform <$> lambda
        -- then
        runIdentity (runLambdaT actual "anything") @?= transform embedded

  ]
