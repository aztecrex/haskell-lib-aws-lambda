module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Data.Functor.Identity (Identity(..), runIdentity)
import Control.Monad.Reader (ask, runReader)
import Control.Applicative (liftA2)
import Control.Monad.Trans.Class (lift)


import Cloud.Compute.AWS.Lambda (runLambdaT, liftLambdaT, runLambda, liftLambda, argument, nogood)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [

    testCase "construct lambdaT from wrapped" $ do
        let embedded = 17
        -- when
            lambda = liftLambdaT (Identity embedded)
        -- then
        runIdentity (runLambdaT lambda "anything") @?= embedded,

    testCase "extract argument" $ do
        let input = "input"
        -- when
            actual = argument
        -- then
        runLambda actual input @?= input,

        testCase "functor" $ do
        let embedded = 19
            transform = (+1)
            lambda = liftLambda embedded
        -- when
            actual = transform <$> lambda
        -- then
        runLambda actual "anything" @?= transform embedded,

    testCase "applicative" $ do
        let embedded1 = 19
            lambda1 = liftLambda embedded1
            embedded2 = 119
            lambda2 = liftLambda embedded2
            op = (+)
        -- when
            actual = liftA2 op lambda1 lambda2
        -- then
        runLambda actual "anything" @?= op embedded1 embedded2,

    testCase "monad return" $ do
        let embedded = 919
        -- when
            actual = return embedded
        -- then
        runLambda actual "anything" @?= embedded,

    testCase "monad bind" $ do
        let embedded = 920
            op = (+ 7)
            orig = pure embedded
            f a = pure (op a)
        -- when
            actual = orig >>= f
        runLambda actual "anything" @?= op embedded,

    testCase "demo" $ do
        let input = 30
            op = (* 100)
        -- when
            actual = do
                x <- argument
                pure $ op x
        runLambda actual input @?= op input,

    testCase "monad trans" $ do
        let inner a = do
                b <- ask
                return (a == b)
            actual = do
                x <- argument
                lift (inner x)
        assertBool "" $ runReader (runLambdaT actual 150) 150
        assertBool "" $ not $ runReader (runLambdaT actual 150) 151,

    testCase "failure" $ do
        let err = "epic fail"
        -- when
            actual = nogood err
        runLambda actual "anything" @?= (Left err :: Either String String)


  ]
