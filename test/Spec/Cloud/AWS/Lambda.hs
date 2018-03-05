module Spec.Cloud.AWS.Lambda (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Aeson (decodeStrict, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Default (def)
import Data.HashMap.Strict (fromList)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
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
        actual @?= invocationv,

    testCase "unmarshall context" $ do
        -- given
        let marshaled = incomingContext
        -- when
            maybeActual = parse marshaled
        -- then
            expected = def {
                lambdaName = incomingName,
                lambdaVersion = incomingVersion,
                lambdaInvocation = incomingInvocation
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

incomingContext :: Text
incomingContext = (decodeUtf8 . toStrict . encode) $ fromList [
    ("lambdaName" :: Text, incomingName),
    ("lambdaVersion" :: Text, incomingVersion),
    ("lambdaInvocation" :: Text, incomingInvocation)
    ]

-- incomingContext :: Text
-- incomingContext = decodeUtf8 (toStrict (encode contextObject))
-- incomingContext =   "{"
--             <>      " \"lambdaName\": \""       <> incomingName <> "\","
--             <>      " \"lambdaVersion\": \""    <> incomingVersion <> "\","
--             <>      " \"lambdaInvocation\": \"" <> incomingInvocation
--             <>      "}"
