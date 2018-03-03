module Spec.Cloud.Compute.Ephemeral (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion, assertFailure)

import Data.Text (Text)

import Cloud.Compute (runCompute)
import Cloud.Compute.Ephemeral (EphemeralInfo (..), name)

tests :: TestTree
tests = testGroup "Ephemeral" [
        testCase "extract name from context" $ do
            -- given
            let namev = "function name"
                ctx = Name namev
            -- when
                actual = name
            -- then
            runCompute actual ctx "any" @?>= namev

    ]


newtype Name = Name { unname :: Text } deriving (Eq, Show)

instance EphemeralInfo Name where
    functionName = unname

assertRight :: (Eq a, Show a) => Either e a -> a -> Assertion
assertRight (Right actual) expected = actual @?= expected
assertRight _ _ = assertFailure "should be Right"

(@?>=) :: (Eq a, Show a) => Either e a -> a -> Assertion
(@?>=) = assertRight

-- assertLeft :: (Eq e, Show e) => Either e a -> e -> Assertion
-- assertLeft (Left actual) expected = actual @?= expected
-- assertLeft _ _ = assertFailure "should be Left"

-- (@?<=) :: (Eq e, Show e) => Either e a -> e -> Assertion
-- (@?<=) = assertLeft
