module Main where

import Test.Tasty

import qualified ConversionTest as Conversion

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Conversion.tests]
