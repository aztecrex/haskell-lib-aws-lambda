module Spec.TestHelp (
    assertLeft,
    assertRight,
    (@?>=),
    (@?<=)
) where

import Test.Tasty.HUnit ((@?=), Assertion, assertFailure)

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
