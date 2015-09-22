module Main where

import Test.Tasty

import qualified ConversionTest as Conversion
import qualified ConfigParserTest as ConfigParser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Conversion.tests, ConfigParser.tests]
