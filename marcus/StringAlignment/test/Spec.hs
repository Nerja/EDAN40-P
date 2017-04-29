import Test.Tasty
import Test.Tasty.HUnit
import Lib
import Data.List
import Data.Hashable

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

optAlignments'Test :: TestTree
optAlignments'Test = testGroup "Unit tests for opt optAlignments'"
  [
      testCase "align \"\" with \"\"" $ optAlignments' 1 1 1 "" "" @?= [("","")]
    , testCase "align Hejsan with \"\"" $ optAlignments' 1 2 3 "Hejsan" "" @?= [("Hejsan", "------")]
    , testCase "align \"\" with troll" $ optAlignments' 3 2 1 "" "troll" @?= [("-----", "troll")]
    , testCase "align Fix with Fix" $ optAlignments' 1 (-1) (-1) "Fix" "Fix" @?= [("Fix", "Fix")]
    , testCase "Given case" $ (sort $ optAlignments' 0 (-1) (-1) "writers" "vintner") @?= (sort [("writ-ers","vintner-"), ("wri-t-ers","-vintner-"), ("wri-t-ers","v-intner-")])
    , testCase "Given long case" $ (hash $ sort $ optAlignments' 0 (-1) (-1) "aferociousmonadatemyhamster" "functionalprogrammingrules") @?= -9072215712528480068
    , testCase "Given long case2" $ (hash $ sort $ optAlignments' 1 (-1) (-2) "bananrepubliksinvasionsarmestabsadjutant" "kontrabasfiolfodralmakarmästarlärling") @?= -1599335011229055460
  ]

outputOptAlignmentsTest :: TestTree
outputOptAlignmentsTest = testGroup "Unit tests for outputOptAlignments"
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
    , outputOptAlignmentsTest
    , similarityScore'Test
    , optAlignments'Test
  ]

main = defaultMain unitTests
