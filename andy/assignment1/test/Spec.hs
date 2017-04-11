import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List.Utils
import Chatterbot

-- Tests for the substitute function
substituteTest :: TestTree
substituteTest = testGroup "Unit tests for substitute"
  [
      testCase "Given testcase" $ (substitute 'x' "3*cos(x) + 4 - x" "5.37") @?= "3*cos(5.37) + 4 - 5.37"
    , testCase "Extra" $ (substitute 1 [1,2,3,1,3] []) @?= [2,3,3]
    , testCase "Empty" $ substitute 1 [] [1, 2, 3] @?= []
  ]

-- Tests for match function
matchTest :: TestTree
matchTest = testGroup "Unit tests for match"
  [
      testCase "Single match" $ match 'x' "2*x+3" "2*7+3" @?= Just "7"
    , testCase "Plain diff words" $ match '*' "frodo" "gandalf" @?= Nothing
    , testCase "Numbers" $ match 2 [1,3..5] [1,3..5] @?= Just []
    , testCase "Finds you" $ match '*' "* and *" "you and me" @?= Just "you"
    , testCase "No match clearly" $ match 'x' "2*x+3+x" "2*7+3" @?= Nothing
    , testCase "Finds b" $ match '*' "*do" "bdo" @?= Just "b"
    , testCase "Finds bode" $ match '*' "*do" "dobedo" @?= Just "dobe"
    , testCase "different endings" $ match '*' "*do" "bedobe" @?= Nothing
    , testCase "Clearly matching" $ match '*' "" "" @?= Just []
    , testCase "Clearly not matching" $ match '*' "abba" "" @?= Nothing
    , testCase "Clearly not matching" $ match '*' "" "abba" @?= Nothing
    , testCase "Empty matching" $ match '*' "a" "a" @?= Just []
    , testCase "Single matching letter" $ match '*' "*" "a" @?= Just "a"
    , testCase "Matching word" $ match '*' "*" "abba" @?= Just "abba"
    , testCase "Matching 1 beginning" $ match '*' "*X*" "aXb" @?= Just "a"
    , testCase "Matching one ending" $ match '*' "*X*" "aaXbb" @?= Just "aa"
  ]

singleWildcardMatchTest :: TestTree
singleWildcardMatchTest = testGroup "Unit tests for singleWildcardMatch"
  [
      testCase "Case 1, instructions" $ singleWildcardMatch "*do" "bdo" @?= Just "b"
    , testCase "Case 2, instructions" $ singleWildcardMatch "*do" "dobedo" @?= Nothing
    , testCase "Case 3, instructions" $ singleWildcardMatch "*do" "bedobe" @?= Nothing
  ]

longerWildcardMatchTest :: TestTree
longerWildcardMatchTest = testGroup "Unit tests for longerWildcardMatch"
  [
      testCase "Case 1, instructions" $ longerWildcardMatch "*do" "bdo" @?= Nothing
    , testCase "Case 2, instructions" $ longerWildcardMatch "*do" "dobedo" @?= Just "dobe"
    , testCase "Case 3, instructions" $ longerWildcardMatch "*do" "bedobe" @?= Nothing
  ]

frenchPresentation = ("My name is *", "Je m'appelle *")
transformationApplyTest :: TestTree
transformationApplyTest = testGroup "Unit tests for transformationApply"
  [
      testCase "Given testcase in instructions" $ transformationApply '*' id "My name is Zacharias" frenchPresentation
        @?= Just "Je m'appelle Zacharias"
    , testCase "Testcase from testfile" $ transformationApply '*' id "My shoe size is 45" frenchPresentation
        @?= Nothing
    , testCase "Empty input" $ transformationApply '*' id "" frenchPresentation
        @?= Nothing
    , testCase "Empty transformation" $ transformationApply '*' id "My name is Zacharias" ("", "")
        @?= Nothing
    , testCase "Empty input and transformation" $ transformationApply '*' id "" ("", "")
        @?= Just ""
  ]

swedishPresentation = ("My name is *", "Mitt namn är *")
presentations = [frenchPresentation, swedishPresentation]
transformationsApplyTest :: TestTree
transformationsApplyTest = testGroup "Unit tests for transformationsApply"
  [
      testCase "Given testcase nbr1" $ transformationsApply '*' id presentations "My name is Zacharias"
        @?= Just "Je m'appelle Zacharias"
    , testCase "Given testcase nbr2" $ transformationsApply '*' id (reverse presentations) "My name is Zacharias"
        @?= Just "Mitt namn är Zacharias"
    , testCase "Given testcase nbr3" $ transformationsApply '*' id (reverse presentations) "My shoe size is 45"
        @?= Nothing
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
      substituteTest
    , matchTest
    , singleWildcardMatchTest
    , longerWildcardMatchTest
    , transformationApplyTest
    , transformationsApplyTest
  ]

substituteAlt :: Char -> [Char] -> [Char] -> Bool
substituteAlt wild list rep = substitute wild list rep == replace [wild] rep list

propertyTests :: TestTree
propertyTests = testGroup "All property tests"
  [
      testProperty "substitute same as MissingH replace" substituteAlt
  ]

allTests = testGroup "All tests"
  [
      unitTests
    , propertyTests
  ]

main = defaultMain allTests
