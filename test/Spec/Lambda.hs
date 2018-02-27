module Spec.Lambda (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, Assertion, assertFailure)

import Data.Functor.Identity (Identity(..), runIdentity)
import Control.Monad.Reader (ask, runReader)
import Control.Applicative (liftA2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)


import Cloud.Compute.AWS.Lambda ()
import Cloud.Compute (ComputeT, runCompute)


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

    ]
