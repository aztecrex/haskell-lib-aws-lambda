module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Data.Functor.Identity (Identity(..), runIdentity)
import Control.Monad.Reader (ask, runReader)
import Control.Applicative (liftA2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)


import Cloud.Compute.AWS.Lambda (runLambdaT, liftLambdaT, runLambda, liftLambda, argument, nogood, LambdaT)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [

    testCase "construct lambdaT from wrapped" $ do
        let embedded = 17
        -- when
            lambda = liftLambdaT (Identity embedded)
        -- then
        runIdentity (runLambdaT lambda "anything") @?= (Right embedded :: Either String Int),

    testCase "extract argument" $ do
        let input = "input"
        -- when
            actual = argument
        -- then
        runLambda actual input @?= (Right input :: Either String String),

    testCase "functor" $ do
        let embedded = 19
            transform = (+1)
            lambda = liftLambda embedded
        -- when
            actual = transform <$> lambda
        -- then
        runLambda actual "anything" @?= (Right (transform embedded) :: Either String Int),

    testCase "failure" $ do
        let err = "epic fail"
        -- when
            actual = nogood err
        runLambda actual "anything" @?= (Left err :: Either String String),

        testCase "applicative" $ do
        let embedded1 = 19
            lambda1 = liftLambda embedded1
            embedded2 = 119
            lambda2 = liftLambda embedded2
            op = (+)
        -- when
            actual = liftA2 op lambda1 lambda2
        -- then
        runLambda actual "anything" @?= (Right (op embedded1 embedded2) :: Either String Int),

    testCase "monad return" $ do
        let embedded = 919
        -- when
            actual = return embedded
        -- then
        runLambda actual "anything" @?= (Right embedded :: Either String Int),

    testCase "monad bind (continue)" $ do
        let embedded = 920
            op = (+ 7)
            orig = pure embedded
            f a = pure (op a)
        -- when
            actual = orig >>= f
        runLambda actual "anything" @?= (Right (op embedded) :: Either String Int),

    testCase "monad bind (failed)" $ do
        let err = "stop here"
            orig = nogood err
            op = (* 7)
            f a = pure (op a)
        -- when
            actual = orig >>= f
        runLambda actual "anything" @?= Left err,

    testCase "monad trans" $ do
        let inner a = do
                b <- ask
                return (a == b)
            program = do
                x <- argument
                lift (inner x)
        runReader (runLambdaT program 150) 150 @?= (Right True :: Either String Bool)
        runReader (runLambdaT program 150) 151 @?= (Right False :: Either String Bool),

    testCase "monad IO" $ do
        let v = 131
            op = (* 17)
            program :: LambdaT String String IO Int = do
                x <- liftIO (pure v)
                pure $ op x
        assertBool "" True, -- compile-only test

    testCase "demo" $ do
        let input = 30
            op = (* 100)
        -- when
            actual = do
                x <- argument
                pure $ op x
        runLambda actual input @?= (Right (op input) :: Either String Int)

  ]
