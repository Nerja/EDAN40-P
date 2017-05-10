import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Parser

-- Tests for Parser.letter
letterTest :: TestTree
letterTest = testGroup "Unit tests for letter"
  [
      testCase "\"abc\"" $ letter "abc" @?= Just('a', "bc")
    , testCase "\"123\"" $ letter "123" @?= Nothing
    , testCase "\"\"" $ letter "" @?= Nothing
  ]

-- Tests for Parser.spaces
spacesTest :: TestTree
spacesTest = testGroup "Unit tests for spaces"
  [
      testCase "\"abc\"" $ spaces "abc" @?= Just("", "abc")
    , testCase "\"  \t abc\"" $ spaces "  \t abc" @?= Just("  \t ","abc")
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
      letterTest
    , spacesTest
  ]

allTests = testGroup "All tests"
  [
      unitTests
  ]

main = defaultMain allTests
