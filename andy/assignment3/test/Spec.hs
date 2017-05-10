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

-- Tests for Parser.chars
charsTest :: TestTree
charsTest = testGroup "Unit tests for chars"
  [
      testCase "2 \"abc\"" $ chars 2 "abc" @?= Just("ab", "c")
    , testCase "0 \"ab\"" $ chars 0 "ab" @?= Just("", "ab")
    , testCase "3 \"ab\"" $ chars 3 "ab" @?= Nothing
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
      letterTest
    , spacesTest
    , charsTest
  ]

allTests = testGroup "All tests"
  [
      unitTests
  ]

main = defaultMain allTests
