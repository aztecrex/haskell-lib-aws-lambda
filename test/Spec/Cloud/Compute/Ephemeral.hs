module Spec.Cloud.Compute.Ephemeral (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Spec.TestHelp ((@?>=))

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

