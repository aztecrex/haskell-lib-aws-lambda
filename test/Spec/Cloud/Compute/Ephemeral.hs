{-# OPTIONS_GHC -fno-warn-orphans #-}
-- allowing orphans in tests, being as specific as possible and disallowing overlapping instances
-- did use a newtype and it was kind of noisy but may decide to put it back


module Spec.Cloud.Compute.Ephemeral (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Spec.TestHelp ((@?>=))

import Control.Monad.Time (MonadTime (..))
import Control.Monad.Trans.Reader (runReader, ReaderT, ask)
import Data.Text (Text)
import Data.Time.Calendar (Day (ModifiedJulianDay) )
import Data.Time.Clock (UTCTime (..), diffUTCTime)

import Cloud.Compute (runCompute, runComputeT)
import Cloud.Compute.Ephemeral (
    OperationContext (..), TimedOperationContext (..),
    name, version, invocation, deadline, remainingTime)

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
            runCompute actual ctx "any" @?>= invocationv,

        testCase "extract deadline" $ do
            -- given
            let deadlinev = UTCTime (ModifiedJulianDay 12001) 20
                ctx = Deadline deadlinev
            -- when
                actual = deadline
            -- then
            runCompute actual ctx "any" @?>= deadlinev,

        testCase "compute remaining time" $ do
            -- given
            let deadlinev = UTCTime (ModifiedJulianDay 12500) 19
                now = UTCTime (ModifiedJulianDay 12300) 773
                ctx = Deadline deadlinev
            -- when
                actual = remainingTime
            -- then
                expected = diffUTCTime deadlinev now
            runReader (runComputeT actual ctx "any") now @?>= expected

      ]

newtype Name = Name { unname :: Text } deriving (Eq, Show)
instance OperationContext Name where
    operationName = unname
    operationVersion = error "not implemented"
    operationInvocation = error "not implemented"

newtype Version = Version { unversion :: Text } deriving (Eq, Show)
instance OperationContext Version where
    operationVersion = unversion
    operationName = error "not implemented"
    operationInvocation = error "not implemented"

newtype Invocation = Invocation { uninvocation :: Text } deriving (Eq, Show)
instance OperationContext Invocation where
    operationInvocation = uninvocation
    operationName = error "not implemented"
    operationVersion = error "not implemented"

newtype Deadline = Deadline { undeadline :: UTCTime } deriving (Eq, Show)
instance TimedOperationContext Deadline where
    operationDeadline = undeadline

instance (Monad m) => MonadTime (ReaderT UTCTime m) where
    currentTime = ask

