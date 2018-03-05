module Spec.Cloud.AWS.Lambda (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Default (def)
import Cloud.Compute.Ephemeral (OperationContext (..))

import Cloud.AWS.Lambda (LambdaContext (..))

tests :: TestTree
tests = testGroup "Lambda" [
    testCase "context operation name" $ do
        -- given
        let namev = "lambda function name"
            ctx = def { lambdaName = namev }
        -- when
            actual = operationName ctx
        -- then
        actual @?= namev,

    testCase "context operation version" $ do
        -- given
        let versionv = "lambda version name"
            ctx = def { lambdaVersion = versionv }
        -- when
            actual = operationVersion ctx
        -- then
        actual @?= versionv,

    testCase "context operation invocation" $ do
        -- given
        let invocationv = "lambda invocation id"
            ctx = def { lambdaInvocation = invocationv }
        -- when
            actual = operationInvocation ctx
        -- then
        actual @?= invocationv

    ]
