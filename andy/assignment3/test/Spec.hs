import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Parser

-- Tests for Parser.letter
letterTest :: TestTree
letterTest = testGroup "Unit tests for letter"
  [
      testCase "Letter \"abc\"" $ letter "abc" @?= Just ('a', "bc")
    , testCase "Letter \"123\"" $ letter "123" @?= Nothing
    , testCase "Letter \"\"" $ letter "" @?= Nothing 
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
      letterTest
  ]

allTests = testGroup "All tests"
  [
      unitTests
  ]

main = defaultMain allTests
