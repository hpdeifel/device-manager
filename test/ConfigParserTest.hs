{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Data.Config.Parser module
module ConfigParserTest (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Config.Parser
import           Data.Text (Text)
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Config File Parser" [shouldSucceed, shouldFail]

shouldSucceed :: TestTree
shouldSucceed = testGroup "Successfully"
  [ testCase "Parses empty string" $
      parseConfig "" @?= Right (ConfigFile [])


  , testCase "Parses a single section" $
      parseConfig "[section]" @?= Right (ConfigFile [ Section "section" [] ])


  , testCase "Parses two consecutive sections" $
      let example = T.unlines [ "[ section1 ]"
                              , "[section2]"
                              ]
          expected = Right $ ConfigFile [ Section "section1" []
                                        , Section "section2" []
                                        ]
      in parseConfig example @?= expected


  , testCase "Parses a section with a variable assignment" $
      let example = T.unlines [ "[section]"
                              , "var = value"
                              ]
          expected = Right (ConfigFile [ Section "section" [ Assignment "var" "value" ] ])
      in parseConfig example @?= expected

  , testCase "Parses a single comment" $
      parseConfig " # foobar\n " @?= Right (ConfigFile [])

  , testCase "Parses complete example" $
      parseConfig completeExample @?= Right completeExampleParsed
  ]

shouldFail :: TestTree
shouldFail = testGroup "Fails to"
  [ testCase "Parse an empty section name" $
      failsToParse "[]" (Just "section name")

  , testCase "Parse an unbanlanced bracket" $
      failsToParse "[ foobar" (Just "]")

  , testCase "Parse a variable name with spaces" $
      failsToParse "[ blah ]\nfoo bar = baz" (Just "=")
  ]

completeExample :: Text
completeExample = T.unlines
  [ "#comment"
  , "[ section1 ]"
  , "var1 = foo "
  , ""
  , "# comment"
  , "[section2]"
  , "var2 = bar # comment"
  ]

completeExampleParsed :: ConfigFile
completeExampleParsed =
  ConfigFile [ Section "section1" [Assignment "var1" "foo"]
             , Section "section2" [Assignment "var2" "bar"]
             ]

failsToParse :: Text -> Maybe Text -> Assertion
failsToParse txt expecting = case (parseConfig txt, expecting) of
  (Right res, _) -> assertFailure $ "Successfully parsed: " ++ show res
  (Left _, Nothing) -> return ()
  (Left err, Just expected) ->
    let expContained = expected `T.isInfixOf` T.pack (show err)
    in flip assertBool expContained $
         "No " ++ show expected ++ " in " ++ show err
