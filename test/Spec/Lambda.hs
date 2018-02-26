module Spec.Lambda (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, Assertion, assertFailure)

import Data.Functor.Identity (Identity(..), runIdentity)
import Control.Monad.Reader (ask, runReader)
import Control.Applicative (liftA2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)


import Cloud.Compute.AWS.Lambda (runLambdaT, liftLambdaT, runLambda, liftLambda, argument, context, nogood, LambdaT, OperationInfo (..))


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


tests :: TestTree
tests = testGroup "Lambda" [

    testCase "construct lambdaT from inner" $ do
        let embedded = 17
        -- when
            lambda = liftLambdaT (Identity embedded)
        -- then
        runIdentity (runLambdaT lambda "anyc" "anyi") @?>= embedded,

    testCase "extract argument" $ do
        let input = "input"
        -- when
            actual = argument
        -- then
        runLambda actual "any" input @?>= input,

    testCase "extract context" $ do
        -- given
        let contextV = "a context"
        -- when
            actual = context
        -- then
        runLambda actual contextV "anyi" @?>= contextV,

    testCase "extract operation name" $ do
        -- given
        let name = "name"
        -- when
            actual = operationName
        -- then
        runLambda actual (Name name) "any" @?>= name,

    testCase "extract operation version" $ do
        -- given
        let version = "version"
        -- when
            actual = operationVersion
        -- then
        runLambda actual (Version version) "any" @?>= version,

    testCase "functor" $ do
        let embedded = 19
            transform = (+1)
            lambda = liftLambda embedded
        -- when
            actual = transform <$> lambda
        -- then
        runLambda actual "anyc" "anyi" @?>= transform embedded,

    testCase "failure" $ do
        let err = "epic fail"
        -- when
            actual = nogood err
        runLambda actual "anyc" "anyi" @?<= err,

    testCase "applicative" $ do
    let embedded1 = 19
        lambda1 = liftLambda embedded1
        embedded2 = 119
        lambda2 = liftLambda embedded2
        op = (+)
    -- when
        actual = liftA2 op lambda1 lambda2
    -- then
    runLambda actual "anyc" "anyi" @?>= op embedded1 embedded2,

    testCase "monad return" $ do
        let embedded = 919
        -- when
            actual = return embedded
        -- then
        runLambda actual "anyc" "anyi" @?>= embedded,

    testCase "monad bind (continue)" $ do
        let embedded = 920
            op = (+ 7)
            orig = pure embedded
            f a = pure (op a)
        -- when
            actual = orig >>= f
        runLambda actual "anyc" "anyi" @?>= op embedded,

    testCase "monad bind (failed)" $ do
        let err = "stop here"
            orig = nogood err
            op = (* 7)
            f a = pure (op a)
        -- when
            actual = orig >>= f
        runLambda actual "anyc" "anyi" @?<= err,

    testCase "monad trans" $ do
        let inner a = do
                b <- ask
                return (a == b)
            program = do
                x <- argument
                lift (inner x)
        runReader (runLambdaT program "any" 150) 150 @?>= True
        runReader (runLambdaT program "any" 150) 151 @?>= False,

    testCase "monad IO" $ do
        let v = 131
            op = (* 17)
            program :: LambdaT String String String IO Int = do
                x <- liftIO (pure v)
                pure $ op x
        runLambdaT program "anyc" "anyi" >> pure (), -- compilation is verification, this just runs it

    testCase "demo" $ do
        let input = 30
            op = (* 100)
        -- when
            actual = do
                x <- argument
                pure $ op x
        runLambda actual "anyc" input @?>= op input

    ]

newtype Name = Name String deriving (Eq, Show)
newtype Version = Version String deriving (Eq, Show)

instance (Monad m) => OperationInfo (LambdaT Name evt err m) where
    operationName = unwrap <$> context
        where unwrap (Name v) = v
    operationVersion = pure "this op has no version"

instance (Monad m) => OperationInfo (LambdaT Version evt err m) where
    operationName = pure "this op has no name"
    operationVersion = unwrap <$> context
        where unwrap (Version v) = v

