import Test.Tasty
import Test.Tasty.HUnit
import Lib
import Data.List

similarityScoreTest :: TestTree
similarityScoreTest = testGroup "Unit tests for similarityScore"
  [
      testCase "HASKELL and PASCAL" $ similarityScore 1 (-1) (-2) "HASKELL" "PASCAL" @?= -2
    , testCase "\"\" with \"\"" $ similarityScore 1 (-1) (-2) "" "" @?= 0
    , testCase "writers and vintner" $ similarityScore 0 (-1) (-1) "writers" "vintner" @?= -5
  ]

maximaByTest :: TestTree
maximaByTest = testGroup "Unit tests for maximaByTest"
  [
      testCase "maximum length strings" $ maximaBy length ["cs", "efd", "lth", "it"] @?= ["efd","lth"]
    , testCase "maximum length empty" $ maximaBy length [[1]] @?= [[1]]
    , testCase "maximum length single" $ maximaBy length ["hehe", "trolllol", "hehe"] @?= ["trolllol"]
    , testCase "maximum length all" $ maximaBy length ["grill", "kaffe", "nerja"] @?= ["grill", "kaffe", "nerja"]
  ]

optAlignmentsTest :: TestTree
optAlignmentsTest = testGroup "Unit tests for optAlignments"
  [
      testCase "align \"\" with \"\"" $ optAlignments 1 1 1 "" "" @?= [("","")]
    , testCase "align Hejsan with \"\"" $ optAlignments 1 2 3 "Hejsan" "" @?= [("Hejsan", "------")]
    , testCase "align \"\" with troll" $ optAlignments 3 2 1 "" "troll" @?= [("-----", "troll")]
    , testCase "align Fix with Fix" $ optAlignments 1 (-1) (-1) "Fix" "Fix" @?= [("Fix", "Fix")]
    , testCase "Given case" $ (sort $ optAlignments 0 (-1) (-1) "writers" "vintner") @?= (sort [("writ-ers","vintner-"), ("wri-t-ers","-vintner-"), ("wri-t-ers","v-intner-")])
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
      similarityScoreTest
    , maximaByTest
    , optAlignmentsTest
  ]

main = defaultMain unitTests
