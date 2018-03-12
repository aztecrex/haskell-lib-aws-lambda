module Spec.Cloud.Compute (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Spec.TestHelp ((@?>=), (@?<=))

import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Ratio ((%))
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Control.Monad.Reader (ask, runReader)
import Control.Applicative (liftA2)
import Control.Monad.Time (MonadTime (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import Cloud.Compute (runComputeT, runCompute, event, context, abort, ComputeT)

import qualified Spec.Cloud.Compute.Ephemeral as Ephemeral (tests)

tests :: TestTree
tests = testGroup "Compute" [

    testCase "construct lambdaT from inner" $ do
        let embedded = 17
        -- when
            lambda = lift (Identity embedded)
        -- then
        runIdentity (runComputeT lambda "anyc" "anyi") @?>= embedded,

    testCase "extract event" $ do
        let input = "input"
        -- when
            actual = event
        -- then
        runCompute actual "any" input @?>= input,

    testCase "extract context" $ do
        -- given
        let contextV = "a context"
        -- when
            actual = context
        -- then
        runCompute actual contextV "anyi" @?>= contextV,

    testCase "abort" $ do
        let err = "epic fail"
        -- when
            actual = abort err
        runCompute actual "anyc" "anyi" @?<= err,

    testCase "change event" $ do
        -- given
        let orig = 10011
            replacement = "cat burglar"
        -- when
            actual = withEvent replacement event
        -- then
        runCompute actual orig "any" @?>= replacement,

    testCase "functor" $ do
        let embedded = 19
            transform = (+1)
            lambda = pure embedded
        -- when
            actual = transform <$> lambda
        -- then
        runCompute actual "anyc" "anyi" @?>= transform embedded,

    testCase "applicative" $ do
    let embedded1 = 19
        lambda1 = pure embedded1
        embedded2 = 119
        lambda2 = pure embedded2
        op = (+)
    -- when
        actual = liftA2 op lambda1 lambda2
    -- then
    runCompute actual "anyc" "anyi" @?>= op embedded1 embedded2,

    testCase "monad return" $ do
        let embedded = 919
        -- when
            actual = return embedded
        -- then
        runCompute actual "anyc" "anyi" @?>= embedded,

    testCase "monad bind (continue)" $ do
        let embedded = 920
            op = (+ 7)
            orig = pure embedded
            f a = pure (op a)
        -- when
            actual = orig >>= f
        runCompute actual "anyc" "anyi" @?>= op embedded,

    testCase "monad bind (failed)" $ do
        let err = "stop here"
            orig = abort err
        -- when
            actual = orig >> pure ()
        runCompute actual "anyc" "anyi" @?<= err,

    testCase "monad trans" $ do
        let inner a = do
                b <- ask
                return (a == b)
            program = do
                x <- event
                lift (inner x)
        runReader (runComputeT program "any" 150) 150 @?>= True
        runReader (runComputeT program "any" 150) 151 @?>= False,

    testCase "monad IO" $ do
        let v = 131
            op = (* 17)
            program :: ComputeT String String String IO Int = do
                x <- liftIO (pure v)
                pure $ op x
        runComputeT program "anyc" "anyi" >> pure (), -- compilation is verification, this just runs it

    testCase "monad time" $ do
        -- given
        let now = posixSecondsToUTCTime $ fromRational (1000000000 % 337)
        -- when
            actual = currentTime
        --then
        runTime (runComputeT actual "any" "any") now @?>= now,

    testCase "demo" $ do
        let input = 30
            op = (* 100)
        -- when
            actual = do
                x <- event
                pure $ op x
        runCompute actual "anyc" input @?>= op input,

    Ephemeral.tests

    ]

withEvent :: (Monad m) => evt' -> ComputeT ctx evt' err m a -> ComputeT ctx evt err m a
withEvent repl compute = do
    ctx <- context
    maybeAnswer <- lift $ runComputeT compute ctx repl
    case maybeAnswer of
        Right x -> pure x
        Left e -> abort e

newtype Time a = Time { runTime :: UTCTime -> a } deriving (Functor, Applicative, Monad)

get :: Time UTCTime
get = Time id

instance MonadTime Time where
    currentTime = get
