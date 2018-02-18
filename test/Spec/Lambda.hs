module Spec.Lambda (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, Assertion, assertFailure)

import Data.Functor.Identity (Identity(..), runIdentity)
import Control.Monad.Reader (ask, runReader)
import Control.Applicative (liftA2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)


import Cloud.Compute.AWS.Lambda (runLambdaT, liftLambdaT, runLambda, liftLambda, argument, nogood, LambdaT)


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
        runIdentity (runLambdaT lambda "anything") @?>= embedded,

    testCase "extract argument" $ do
        let input = "input"
        -- when
            actual = argument
        -- then
        runLambda actual input @?>= input,

    testCase "functor" $ do
        let embedded = 19
            transform = (+1)
            lambda = liftLambda embedded
        -- when
            actual = transform <$> lambda
        -- then
        runLambda actual "anything" @?>= transform embedded,

    testCase "failure" $ do
        let err = "epic fail"
        -- when
            actual = nogood err
        runLambda actual "anything" @?<= err,

        testCase "applicative" $ do
        let embedded1 = 19
            lambda1 = liftLambda embedded1
            embedded2 = 119
            lambda2 = liftLambda embedded2
            op = (+)
        -- when
            actual = liftA2 op lambda1 lambda2
        -- then
        runLambda actual "anything" @?>= op embedded1 embedded2,

    testCase "monad return" $ do
        let embedded = 919
        -- when
            actual = return embedded
        -- then
        runLambda actual "anything" @?>= embedded,

    testCase "monad bind (continue)" $ do
        let embedded = 920
            op = (+ 7)
            orig = pure embedded
            f a = pure (op a)
        -- when
            actual = orig >>= f
        runLambda actual "anything" @?>= op embedded,

    testCase "monad bind (failed)" $ do
        let err = "stop here"
            orig = nogood err
            op = (* 7)
            f a = pure (op a)
        -- when
            actual = orig >>= f
        runLambda actual "anything" @?<= err,

    testCase "monad trans" $ do
        let inner a = do
                b <- ask
                return (a == b)
            program = do
                x <- argument
                lift (inner x)
        runReader (runLambdaT program 150) 150 @?>= True
        runReader (runLambdaT program 150) 151 @?>= False,

    testCase "monad IO" $ do
        let v = 131
            op = (* 17)
            program :: LambdaT String String IO Int = do
                x <- liftIO (pure v)
                pure $ op x
        runLambdaT program "anything" >> pure (), -- compilation is verification, this just runs it

    testCase "demo" $ do
        let input = 30
            op = (* 100)
        -- when
            actual = do
                x <- argument
                pure $ op x
        runLambda actual input @?>= op input

    ]
