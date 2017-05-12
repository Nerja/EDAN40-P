import Parser
import Dictionary
import Expr
import Statement
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad
import Control.Exception
import Test.Hspec
import Program
import TestPrograms

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

parseTest :: TestTree
parseTest = testGroup "all unit tests fr Task 3.b"
  [
      testCase "Parse assignment" $ toString (fromString "count := 0;" :: Statement.T) @?= "count := 0;"
    , testCase "Parse skip" $ toString (fromString "skip;" :: Statement.T) @?= "skip;"
    , testCase "Parse read" $ toString (fromString "read count;" :: Statement.T) @?= "read count;"
    , testCase "Parse write" $ toString (fromString "write count+1;" :: Statement.T) @?= "write count+1;"
    , testCase "Parse if" $ toString (fromString "if x then skip; else x:=0-x;" :: Statement.T) @?= "if x then skip; else x := 0-x;"
    , testCase "Parse while" $ toString (fromString "while n do n:=n-1;" :: Statement.T) @?= "while n do n := n-1;"
    , testCase "Empty begin end" $ toString (fromString "begin end" :: Statement.T) @?= "begin end"
    , testCase "Simple begin end" $ toString (fromString "begin skip; end" :: Statement.T) @?= "begin skip; end"
    , testCase "Parse begin end given case" $ toString (fromString "begin read x ; x := x + 1 ; write x; end" :: Statement.T) @?= "begin read x; x := x+1; write x; end"
  ]

statementTest :: TestTree
statementTest = testGroup "All unit tests for Statement.hs"
  [
      parseTest
  ]

exprTest :: TestTree
exprTest = testGroup "All unit tests for Expr.hs"
  [
    valueTest
  ]

execTest :: TestTree
execTest = testGroup "All unit tests for exec in Program.hs"
  [
      testCase "Given program p applied to input [3,16]" $ Program.exec (fromString p ::Program.T) [3,16] @?= [3,6,9,12,15]
    , testCase "Given program p1 applied to input [1024, 2]" $ Program.exec (fromString p1 ::Program.T) [1024,2] @?= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 10000000000]
    --, testCase "toString of parsed program p should be pretty" $ toString (fromString p ::Program.T) @?= pPretty
  ]

programTest :: TestTree
programTest = testGroup "All unit tests for Program.hs"
  [
    execTest
  ]

unitTests :: TestTree
unitTests = testGroup "All unit tests"
  [
      parserTest
    , exprTest
    , statementTest
    , programTest
  ]

main = defaultMain unitTests
