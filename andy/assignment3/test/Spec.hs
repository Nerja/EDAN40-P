import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec
import Control.Exception
import Parser
import Dictionary
import Expr

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
    , testCase "error" $ evaluate (require "else" "then") `shouldThrow` anyException
  ]

-- Tests for Parser.-#
bindBoardTest :: TestTree
bindBoardTest = testGroup "All unit tests for -#"
  [
      testCase "" $ (accept "Lund" -# word) "Lund University" @?= Just ("University", "")
    , testCase "" $ (accept "Gote" -# word) "Lund University" @?= Nothing
    , testCase "" $ (word -# (accept "Nej")) "Lund University" @?= Nothing
    , testCase "given case" $ (accept "read" -# word) "read count" @?= Just("count","")
    , testCase "Lennart 1" $ (char -# char) "abc" @?= Just('b', "c")
    , testCase "Lennart 2" $ (char -# char) "a" @?= Nothing
  ]

-- Tests for Parser.#-
boardBindTest :: TestTree
boardBindTest = testGroup "All unit tests for #-"
  [
      testCase "" $ (accept "Lund" #- word) "Lund University" @?= Just ("Lund", "")
    , testCase "" $ (accept "Gote" #- word) "Lund University" @?= Nothing
    , testCase "" $ (word #- (accept "Nej")) "Lund University" @?= Nothing
  ]

parserTests :: TestTree
parserTests = testGroup "All Parser.hs tests"
  [
      letterTest
    , spacesTest
    , charsTest
    , requireTest
    , bindBoardTest
    , boardBindTest
  ]

-----------------------------------------------
dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.empty

testValue string = value (fromString string) dict

-- Tests for Expr.value
valueTest :: TestTree
valueTest = testGroup "All unit tests for value"
  [
      testCase "Single value" $ testValue "1" @?= 1
    , testCase "Single variable" $ testValue "x" @?= 1
    , testCase "Simple add" $ testValue "1+3" @?= 4
    , testCase "Simple sub" $ testValue "2-10" @?= (-8)
    , testCase "Simple div" $ testValue "21/7" @?= 3
    , testCase "Simple mult" $ testValue "3*7" @?= 21
    , testCase "Add two variables" $ testValue "x+y" @?= 3
    , testCase "Subtract variables" $ testValue "x-y-y" @?= (-3)
    , testCase "Division by 0" $ evaluate (testValue "1/(2-y)") `shouldThrow` anyException
    , testCase "Undefined variable" $ evaluate (testValue "2+z") `shouldThrow` anyException
  ]

exprTests :: TestTree
exprTests = testGroup "All Expr.hs tests"
  [
      valueTest
  ]

allTests = testGroup "All tests"
  [
      parserTests
    , exprTests
  ]

main = defaultMain allTests
