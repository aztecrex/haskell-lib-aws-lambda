{-# OPTIONS_GHC -fno-warn-orphans #-}
-- allowing orphans in tests, being as specific as possible and disallowing overlapping instances
-- did use a newtype and it was kind of noisy but may decide to put it back


module Spec.Cloud.Compute.Ephemeral (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Spec.TestHelp ((@?>=))

import Control.Monad.Trans.Reader (runReader, ReaderT, ask)
import Data.Text (Text)
import Data.Time.Calendar (Day (ModifiedJulianDay) )
import Data.Time.Clock (UTCTime (..), diffUTCTime)

import Cloud.Compute (runCompute, runComputeT)
import Cloud.Compute.Ephemeral (EphemeralInfo (..), MonadClock (..), CountDown (..), name, version, invocation, remainingTime)

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

        testCase "compute remaining time" $ do
            -- given
            let deadlinev = UTCTime (ModifiedJulianDay 12000) 19
                now = UTCTime (ModifiedJulianDay 12300) 773
                ctx = Deadline deadlinev
            -- when
                actual = remainingTime
            -- then
                expected = diffUTCTime deadlinev now
            runReader (runComputeT actual ctx "any") now @?>= expected

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

newtype Deadline = Deadline { undeadline :: UTCTime } deriving (Eq, Show)
instance CountDown Deadline where
    deadline = undeadline

instance (Monad m) => MonadClock (ReaderT UTCTime m) where
    currentTime = ask

