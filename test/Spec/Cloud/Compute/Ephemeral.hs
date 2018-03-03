module Spec.Cloud.Compute.Ephemeral (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Spec.TestHelp ((@?>=))

import Data.Text (Text)

import Cloud.Compute (runCompute)
import Cloud.Compute.Ephemeral (EphemeralInfo (..), name, version, invocation)

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
            runCompute actual ctx "any" @?>= versionv,

        testCase "extract invocation" $ do
            -- given
            let invocationv = "function invocation"
                ctx = Invocation invocationv
            -- when
                actual = invocation
            -- then
            runCompute actual ctx "any" @?>= invocationv


      ]


newtype Name = Name { unname :: Text } deriving (Eq, Show)
instance EphemeralInfo Name where
    functionName = unname
    functionVersion = error "not implemented"
    functionInvocation = error "not implemented"

newtype Version = Version { unversion :: Text } deriving (Eq, Show)
instance EphemeralInfo Version where
    functionVersion = unversion
    functionName = error "not implemented"
    functionInvocation = error "not implemented"

newtype Invocation = Invocation { uninvocation :: Text } deriving (Eq, Show)
instance EphemeralInfo Invocation where
    functionInvocation = uninvocation
    functionName = error "not implemented"
    functionVersion = error "not implemented"

