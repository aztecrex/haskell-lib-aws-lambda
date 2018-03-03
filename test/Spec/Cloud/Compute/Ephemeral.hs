module Spec.Cloud.Compute.Ephemeral (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Spec.TestHelp ((@?>=))

import Data.Text (Text)

import Cloud.Compute (runCompute)
import Cloud.Compute.Ephemeral (EphemeralInfo (..), name, version)

tests :: TestTree
tests = testGroup "Ephemeral" [
        testCase "extract name" $ do
            -- given
            let namev = "function name"
                ctx = Name namev
            -- when
                actual = name
            -- then
            runCompute actual ctx "any" @?>= namev,

        testCase "extract version" $ do
            -- given
            let versionv = "function version"
                ctx = Version versionv
            -- when
                actual = version
            -- then
            runCompute actual ctx "any" @?>= versionv
    ]


newtype Name = Name { unname :: Text } deriving (Eq, Show)

instance EphemeralInfo Name where
    functionName = unname
    functionVersion = error "not implemented"

newtype Version = Version { unversion :: Text } deriving (Eq, Show)

instance EphemeralInfo Version where
    functionVersion = unversion
    functionName = error "not implemented"

