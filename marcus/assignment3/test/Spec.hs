import Parser
import Dictionary
import Expr
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad
import Control.Exception
import Test.Hspec

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ Prelude.return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)

letterTest :: TestTree
letterTest = testGroup "All unit tests for letter"
  [
      testCase "Empty string" $ letter "" @?= Nothing
    , testCase "Single non letter" $ letter "!" @?= Nothing
    , testCase "Single letter" $ letter "A" @?= Just ('A', "")
    , testCase "Letter in middle" $ letter "!A!" @?= Nothing
    , testCase "Many letters" $ letter "HEJ" @?= Just ('H', "EJ")
  ]

spacesTest :: TestTree
spacesTest = testGroup "All unit tests for spaces"
  [
      testCase "Empty string" $ spaces "" @?= Just ("","")
    , testCase "Some letters" $ spaces "abc" @?= Just ("", "abc")
    , testCase "Some \"spaces\"" $ spaces "  \t\n  takeme" @?= Just ("  \t\n  ", "takeme")
  ]

charsTest :: TestTree
charsTest = testGroup "All unit tests for chars"
  [
      testCase "Take first 2 from 3" $ chars 2 "abc" @?= Just ("ab", "c")
    , testCase "Take 0 from 2" $ chars 0 "ab" @?= Just ("", "ab")
    , testCase "Take too many" $ chars 7 "abcd" @?= Nothing
  ]

requireTest :: TestTree
requireTest = testGroup "All unit tests for require"
  [
      testCase "require \":=\" from \":= 1337\"" $ require ":=" ":= 1337" @?= Just (":=", "1337")
    , testCase "Given case" $ evaluate (require "else" "then") `shouldThrow` anyException
  ]

bindBoardTest :: TestTree
bindBoardTest = testGroup "All unit tests for -#"
  [
      testCase "" $ (accept "Lund" -# word) "Lund University" @?= Just ("University", "")
    , testCase "" $ (accept "Gote" -# word) "Lund University" @?= Nothing
    , testCase "" $ (word -# (accept "Nej")) "Lund University" @?= Nothing
  ]

boardBindTest :: TestTree
boardBindTest = testGroup "All unit tests for -#"
  [
      testCase "" $ (accept "Lund" #- word) "Lund University" @?= Just ("Lund", "")
    , testCase "" $ (accept "Gote" #- word) "Lund University" @?= Nothing
    , testCase "" $ (word #- (accept "Nej")) "Lund University" @?= Nothing
  ]

parserTest :: TestTree
parserTest = testGroup "All unit tests for Parser.hs"
  [
      letterTest
    , spacesTest
    , charsTest
    , requireTest
    , bindBoardTest
    , boardBindTest
  ]

------------------
testValue string = value (fromString string) dict
dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.empty
valueTest :: TestTree
valueTest = testGroup "All unit tests for value"
  [
      testCase "Single number" $ testValue "1337" @?= 1337
    , testCase "Single variable" $ testValue "x" @?= 1
    , testCase "Simple add" $ testValue "1+3" @?= 4
    , testCase "Simple sub" $ testValue "2-10" @?= (-8)
    , testCase "Simple div" $ testValue "21/7" @?= 3
    , testCase "Simple mult" $ testValue "3*7" @?= 21
    , testCase "Add two vars" $ testValue "x+y" @?= 3
    , testCase "Add some vars" $ testValue "x-y-y" @?= (-3)
    , testCase "Div by zero" $ evaluate (testValue "1337 + 1336/0 - 10") `shouldThrow` anyException
    , testCase "Undef var" $ evaluate (testValue "1337 + k - 10") `shouldThrow` anyException
  ]

exprTest :: TestTree
exprTest = testGroup "All unit tests for Expr.hs"
  [
    valueTest
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
      parserTest
    , exprTest
  ]

main = defaultMain unitTests
