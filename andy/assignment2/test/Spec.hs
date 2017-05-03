import Test.Tasty
import Test.Tasty.HUnit
import Lib
import Data.List

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

optAlignmentsTest :: TestTree
optAlignmentsTest = testGroup "Unit tests for optAlignments"
  [
      testCase "align \"\" with \"\"" $ optAlignments "" "" @?= [("","")]
    , testCase "align Hejsan with \"\"" $ optAlignments "Hejsan" "" @?= [("Hejsan", "------")]
    , testCase "align \"\" with troll" $ optAlignments "" "troll" @?= [("-----", "troll")]
    , testCase "align Fix with Fix" $ optAlignments "Fix" "Fix" @?= [("Fix", "Fix")]
    , testCase "Given case" $ (sort $ optAlignments "writers" "vintner") @?= (sort [("writ-ers","vintner-"), ("wri-t-ers","-vintner-"), ("wri-t-ers","v-intner-")])
  ]

similarityScore'Test :: TestTree
similarityScore'Test = testGroup "Unit tests for similarityScore'"
  [
      testCase "Empty string1" $ similarityScore' "" "AAA" @?= -3
    , testCase "Empty string2" $ similarityScore' "AA" "" @?= -2
    , testCase "writers and vintner" $ similarityScore' "writers" "vintner" @?= -5
    , testCase "first given long case" $ similarityScore' "aferociousmonadatemyhamster" "functionalprogrammingrules" @?= -22
    , testCase "second given long case" $ similarityScore' "bananrepubliksinvasionsarmestabsadjutant" "kontrabasfiolfodralmakarmästarlärling" @?= -30
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
      similarityScoreTest
    , maximaByTest
    , optAlignmentsTest
    , similarityScore'Test
  ]

main = defaultMain unitTests
