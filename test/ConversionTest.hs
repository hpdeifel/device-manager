{-# LANGUAGE OverloadedStrings #-}

module ConversionTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import DBus.UDisks2.Internal
import qualified DBus.UDisks2.Types as U

tests :: TestTree
tests = testGroup "Conversions" [errorTests]

errorTests :: TestTree
errorTests = testGroup "Errors"
  [ testCase "Parse simple error type" $
        parseErrorType "org.freedesktop.UDisks2.Error.NotAuthorizedCanObtain" @=? U.ErrorNotAuthorizedCanObtain
  ]
