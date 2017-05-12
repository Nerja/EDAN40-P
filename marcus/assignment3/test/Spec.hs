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
    , testCase "Simple pow" $ testValue "2^3" @?= 8
    , testCase "Pow with mult 3*2^3" $ testValue "3*2^3" @?= 24
    , testCase "Pow with mult reverse" $ testValue "2^3*3" @?= 24
    , testCase "Pow with mult paran" $ testValue "(3*2)^3" @?= 216
    , testCase "Pow with mult paran reverse" $ testValue "2^(3*3)" @?= 512
  ]

parseExprTest :: TestTree
parseExprTest = testGroup "all unit tests for parsing expr"
  [
      testCase "Parse simple pow" $ toString (fromString "2^3" :: Expr.T) @?= "2^3"
    , testCase "Parse pow with mult" $ toString (fromString "3*2^3" :: Expr.T) @?= "3*2^3"
    , testCase "Parse pow with mult reverse" $ toString (fromString "2^3*3" :: Expr.T) @?= "2^3*3"
    , testCase "Parse pow with paran" $ toString (fromString "(3*2)^3" :: Expr.T) @?= "(3*2)^3"
    , testCase "Parse pow with paran reverse" $ toString (fromString "2^(3*3)" :: Expr.T) @?= "2^(3*3)"
  ]

parseTest :: TestTree
parseTest = testGroup "all unit tests fr Task 3.b"
  [
      testCase "Parse assignment" $ toString (fromString "count := 0;" :: Statement.T) @?= "count := 0;\n"
    , testCase "Parse skip" $ toString (fromString "skip;" :: Statement.T) @?= "skip;\n"
    , testCase "Parse read" $ toString (fromString "read count;" :: Statement.T) @?= "read count;\n"
    , testCase "Parse write" $ toString (fromString "write count+1;" :: Statement.T) @?= "write count+1;\n"
    , testCase "Parse if" $ toString (fromString "if x then skip; else x:=0-x;" :: Statement.T) @?= "if x then\n\tskip;\nelse\n\tx := 0-x;\n"
    , testCase "Parse while" $ toString (fromString "while n do n:=n-1;" :: Statement.T) @?= "while n do\n\tn := n-1;\n"
    , testCase "Empty begin end" $ toString (fromString "begin end" :: Statement.T) @?= "begin\nend\n"
    , testCase "Simple begin end" $ toString (fromString "begin skip; end" :: Statement.T) @?= "begin\n\tskip;\nend\n"
    , testCase "Parse begin end given case" $ toString (fromString "begin read x ; x := x + 1 ; write x; end" :: Statement.T) @?= "begin\n\tread x;\n\tx := x+1;\n\twrite x;\nend\n"
    , testCase "Parse comment" $ toString (fromString "-- Edward Blom\n" :: Statement.T) @?= "-- Edward Blom\n"
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
    , parseExprTest
  ]

execTest :: TestTree
execTest = testGroup "All unit tests for exec in Program.hs"
  [
      testCase "Given program p applied to input [3,16]" $ Program.exec (fromString p ::Program.T) [3,16] @?= [3,6,9,12,15]
    , testCase "Given program p1 applied to input [1024, 2]" $ Program.exec (fromString p1 ::Program.T) [1024,2] @?= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 10000000000]
    , testCase "toString of parsed program p should be pretty" $ toString (fromString p ::Program.T) @?= "read k;\nread n;\nm := 1;\nwhile n-m do\n\tbegin\n\t\tif m-m/k*k then\n\t\t\tskip;\n\t\telse\n\t\t\twrite m;\n\t\tm := m+1;\n\tend\n"
    , testCase "toString of parsed program p1 should be pretty" $ toString (fromString p1 ::Program.T) @?= "read n;\nread b;\nm := 1;\ns := 0;\np := 1;\nwhile n do\n\tbegin\n\t\tq := n/b;\n\t\tr := n-q*b;\n\t\twrite r;\n\t\ts := p*r+s;\n\t\tp := p*10;\n\t\tn := q;\n\tend\nwrite s;\n"
    , testCase "the toString of parsed p1 should be the same as toString of (parsing a toString)" $ toString (fromString p1 ::Program.T) @?= toString (fromString (toString (fromString p1 ::Program.T)) ::Program.T)
    , testCase "the toString of parsed p should be the same as toString of (parsing a toString)" $ toString (fromString p ::Program.T) @?= toString (fromString (toString (fromString p ::Program.T)) ::Program.T)
    , testCase "Given program p applied to input [3,16], program with comment" $ Program.exec (fromString pComment ::Program.T) [3,16] @?= [3,6,9,12,15]
    , testCase "Given program p4 applied to input [4,4], program with comment" $ Program.exec (fromString p4 ::Program.T) [4,4] @?= [64, 16, 27, 8, 8, 4, 1, 2, 0]
    , testCase "toString of parsed program p4 should be pretty" $ toString (fromString p4 ::Program.T) @?= "read a;\nread b;\n-- a comment\ns := 3;\nwhile a do\n\tbegin\n\t\tc := a^s;\n\t\td := 2^a;\n\t\twrite c;\n\t\twrite d;\n\t\ta := a-1;\n\tend\nwrite a;\n"

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
