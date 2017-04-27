import Test.Tasty
import Test.Tasty.HUnit
import Lib

similarityScoreTest :: TestTree
similarityScoreTest = testGroup "Unit tests for similarityScore"
  [
      testCase "Empty string1" $ similarityScore "" "AAA" @?= -3
    , testCase "Empty string2" $ similarityScore "AA" "" @?= -2
    , testCase "writers and vintner" $ similarityScore "writers" "vintner" @?= -5
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
      similarityScoreTest
  ]

main = defaultMain unitTests
