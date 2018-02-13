module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Functor.Identity (Identity(..), runIdentity)
import Control.Applicative (liftA2)

import Cloud.Compute.AWS.Lambda (runLambdaT, liftLambdaT, runLambda, liftLambda)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [
    testCase "construct lambdaT" $ do
        let embedded = 17
        -- when
            lambda = liftLambdaT (Identity embedded)
        -- then
        runIdentity (runLambdaT lambda "anything") @?= embedded,
    testCase "functor" $ do
        let embedded = 19
            transform = (+1)
            lambda = liftLambda embedded
        -- when
            actual = transform <$> lambda
        -- then
        runLambda actual "anything" @?= transform embedded,
    testCase "applicative" $ do
        let embedded1 = 19
            lambda1 = liftLambda embedded1
            embedded2 = 119
            lambda2 = liftLambda embedded2
            op = (+)
        -- when
            actual = liftA2 op lambda1 lambda2
        -- then
        runLambda actual "anything" @?= op embedded1 embedded2

  ]
