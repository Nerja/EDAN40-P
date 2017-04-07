import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Chatterbot

-- Tests for the substitute function
substituteTest :: TestTree
substituteTest = testGroup "Unit tests for substitute"
  [
    --testCase "Given testcase" $ (substitute 'x' "3*cos(x) + 4 - x" "5.37") @?= "3*cos(5.37) + 4 - 5.37"
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
    substituteTest
  ]

allTests = testGroup "All tests"
  [
    unitTests
  ]

main = defaultMain allTests
