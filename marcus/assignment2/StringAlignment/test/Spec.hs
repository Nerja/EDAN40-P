import Test.Tasty
import Test.Tasty.HUnit
import Lib

similarityScoreTest :: TestTree
similarityScoreTest = testGroup "Unit tests for similarityScore"
  [
    testCase "HASKELL and PASCAL" $ similarityScore "HASKELL" "PASCAL" @?= -2
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
    similarityScoreTest
  ]

main = defaultMain unitTests
