module Spec.Cloud.AWS.Lambda (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Cloud.Compute.Ephemeral (OperationContext (..), TimedOperationContext (..))
import Data.Aeson (decodeStrict, pairs, (.=))
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Default (def)
import Data.Monoid ((<>))
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Cloud.AWS.Lambda (LambdaContext (..))

tests :: TestTree
tests = testGroup "Lambda" [
    testCase "operation context name" $ do
        -- given
        let namev = "lambda function name"
            ctx = def { lambdaName = namev }
        -- when
            actual = operationName ctx
        -- then
        actual @?= namev,

    testCase "operation context version" $ do
        -- given
        let versionv = "lambda version name"
            ctx = def { lambdaVersion = versionv }
        -- when
            actual = operationVersion ctx
        -- then
        actual @?= versionv,

    testCase "operation context invocation" $ do
        -- given
        let invocationv = "lambda invocation id"
            ctx = def { lambdaInvocation = invocationv }
        -- when
            actual = operationInvocation ctx
        -- then
        actual @?= invocationv,

    testCase "timed operation context deadline" $ do
        -- given
        let deadlinev = 1520447224247
            ctx = def { lambdaDeadline = deadlinev }
        -- when
            actual = operationDeadline ctx
        -- then
            expected = posixSecondsToUTCTime $ fromRational (deadlinev % 1000)
        actual @?= expected,

    testCase "unmarshall context" $ do
        -- given
        let marshaled = incomingContext
        -- when
            maybeActual = parse marshaled
        -- then
            expected = def {
                lambdaName = incomingName,
                lambdaVersion = incomingVersion,
                lambdaInvocation = incomingInvocation,
                lambdaDeadline = incomingDeadline
            }
        maybeActual @?= Just expected

    ]

parse :: Text -> Maybe LambdaContext
parse = decodeStrict . encodeUtf8

incomingName :: Text
incomingName = "FunctionName"

incomingVersion :: Text
incomingVersion = "FunctionVersion"

incomingInvocation :: Text
incomingInvocation = "FunctionInvocation"

incomingDeadline :: Integer
incomingDeadline = 109252223

incomingContext :: Text
incomingContext = (decodeUtf8 . toStrict . encodingToLazyByteString) $ pairs (
        "lambdaName" .= incomingName <>
        "lambdaVersion" .= incomingVersion <>
        "lambdaInvocation" .= incomingInvocation <>
        "lambdaDeadline" .= incomingDeadline
    )
