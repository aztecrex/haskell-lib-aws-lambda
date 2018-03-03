module Spec.Cloud.Compute.Ephemeral (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion, assertFailure)

import Data.Text (Text)

import Cloud.Compute (runCompute, ComputeT, context, Compute)
import Cloud.Compute.Ephemeral (MonadEphemeral (..))

tests :: TestTree
tests = testGroup "Ephemeral" [
        testCase "extract name from context" $ do
            -- given
            let namev = "function name"
                ctx = Name namev
            -- when
                actual = name :: Compute Name Text Text Text
            -- then
            runCompute actual ctx "any" @?>= namev

    ]


newtype Name = Name { unname :: Text } deriving (Eq, Show)

instance (Monad m) => MonadEphemeral (ComputeT Name Text Text m) where
    name = unname <$> context
    version = error "not implemented"
    invocation = error "not implemented"



assertRight :: (Eq a, Show a) => Either e a -> a -> Assertion
assertRight (Right actual) expected = actual @?= expected
assertRight _ _ = assertFailure "should be Right"

(@?>=) :: (Eq a, Show a) => Either e a -> a -> Assertion
(@?>=) = assertRight

assertLeft :: (Eq e, Show e) => Either e a -> e -> Assertion
assertLeft (Left actual) expected = actual @?= expected
assertLeft _ _ = assertFailure "should be Left"

(@?<=) :: (Eq e, Show e) => Either e a -> e -> Assertion
(@?<=) = assertLeft
