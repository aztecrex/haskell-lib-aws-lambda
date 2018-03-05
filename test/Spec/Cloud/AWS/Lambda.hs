module Spec.Cloud.AWS.Lambda (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Default (def)
import Cloud.Compute.Ephemeral (operationName)

import Cloud.AWS.Lambda (lambdaName)

tests :: TestTree
tests = testGroup "Lambda" [
        testCase "context operation name" $ do
            -- given
            let namev = "lambda function name"
                ctx = def { lambdaName = namev }
            -- when
                actual = operationName ctx
            -- then
            actual @?= namev

    ]
