module Main where


import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Spec.Lambda as Lambda (tests)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [
        Lambda.tests
    ]

