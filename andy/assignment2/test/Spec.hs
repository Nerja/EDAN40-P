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


maximaByTest :: TestTree
maximaByTest = testGroup "Unit tests for maximaByTest"
  [
      testCase "maximum length strings" $ maximaBy length ["cs", "efd", "lth", "it"] @?= ["efd","lth"]
    , testCase "maximum length empty" $ maximaBy length [[1]] @?= [[1]]
    , testCase "maximum length single" $ maximaBy length ["hehe", "trolllol", "hehe"] @?= ["trolllol"]
    , testCase "maximum length all" $ maximaBy length ["grill", "kaffe", "nerja"] @?= ["grill", "kaffe", "nerja"]
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
      similarityScoreTest
    , maximaByTest
  ]

main = defaultMain unitTests
