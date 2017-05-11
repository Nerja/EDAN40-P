import Test.Tasty
import Test.Tasty.HUnit
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

-- Tests for Parser.require
requireTest :: TestTree
requireTest = testGroup "Unit tests for require"
  [
      testCase "require \":=\" from \":= 1337\"" $ require ":=" ":= 1337" @?= Just(":=", "1337")
    , testCase "given case 1" $ require ":=" ":= 1" @?= Just(":=","1")
  ]

-- Tests for Parser.-#
bindBoardTest :: TestTree
bindBoardTest = testGroup "All unit tests for -#"
  [
      testCase "" $ (accept "Lund" -# word) "Lund University" @?= Just ("University", "")
    , testCase "" $ (accept "Gote" -# word) "Lund University" @?= Nothing
    , testCase "" $ (word -# (accept "Nej")) "Lund University" @?= Nothing
    , testCase "given case" $ (accept "read" -# word) "read count" @?= Just("count","")
  ]

-- Tests for Parser.#-
boardBindTest :: TestTree
boardBindTest = testGroup "All unit tests for #-"
  [
      testCase "" $ (accept "Lund" #- word) "Lund University" @?= Just ("Lund", "")
    , testCase "" $ (accept "Gote" #- word) "Lund University" @?= Nothing
    , testCase "" $ (word #- (accept "Nej")) "Lund University" @?= Nothing
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
      letterTest
    , spacesTest
    , charsTest
    , requireTest
    , bindBoardTest
    , boardBindTest
  ]

allTests = testGroup "All tests"
  [
      unitTests
  ]

main = defaultMain allTests
