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

similarityScore'Test :: TestTree
similarityScore'Test = testGroup "Unit tests for opt similarityScore'"
  [
      testCase "HASKELL and PASCAL" $ similarityScore' 1 (-1) (-2) "HASKELL" "PASCAL" @?= -2
    , testCase "\"\" with \"\"" $ similarityScore' 1 (-1) (-2) "" "" @?= 0
    , testCase "writers and vintner" $ similarityScore' 0 (-1) (-1) "writers" "vintner" @?= -5
    , testCase "first given long case" $ similarityScore' 0 (-1) (-1) "aferociousmonadatemyhamster" "functionalprogrammingrules" @?= -22
    , testCase "second given long case" $ similarityScore' 0 (-1) (-1) "bananrepubliksinvasionsarmestabsadjutant" "kontrabasfiolfodralmakarmästarlärling" @?= -30
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

outputOptAlignmentsTest :: TestTree
outputOptAlignmentsTest = testGroup "Unit tests for outputOptAlignments"
  [
    testCase "Given case" $ outputOptAlignments 0 (-1) (-1) "writers" "vintner" @?= "w r i t - e r s\nv i n t n e r -\n\nw r i - t - e r s\nv - i n t n e r -\n\nw r i - t - e r s\n- v i n t n e r -\n\n"
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
      similarityScoreTest
    , maximaByTest
    , optAlignmentsTest
    , outputOptAlignmentsTest
    , similarityScore'Test
  ]

main = defaultMain unitTests
