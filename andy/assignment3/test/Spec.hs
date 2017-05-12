import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec
import Control.Exception
import Parser
import Dictionary
import Expr
import Statement

-- Tests for Parser.letter
letterTest :: TestTree
letterTest = testGroup "Unit tests for letter"
  [
      testCase "\"abc\"" $ letter "abc" @?= Just('a', "bc")
    , testCase "\"123\"" $ letter "123" @?= Nothing
    , testCase "\"\"" $ letter "" @?= Nothing
  ]

-- Tests for Parser.spaces
spacesTest :: TestTree
spacesTest = testGroup "Unit tests for spaces"
  [
      testCase "\"abc\"" $ spaces "abc" @?= Just("", "abc")
    , testCase "\"  \t abc\"" $ spaces "  \t abc" @?= Just("  \t ","abc")
  ]

-- Tests for Parser.chars
charsTest :: TestTree
charsTest = testGroup "Unit tests for chars"
  [
      testCase "2 \"abc\"" $ chars 2 "abc" @?= Just("ab", "c")
    , testCase "0 \"ab\"" $ chars 0 "ab" @?= Just("", "ab")
    , testCase "3 \"ab\"" $ chars 3 "ab" @?= Nothing
  ]

-- Tests for Parser.require
requireTest :: TestTree
requireTest = testGroup "Unit tests for require"
  [
      testCase "require \":=\" from \":= 1337\"" $ require ":=" ":= 1337" @?= Just(":=", "1337")
    , testCase "given case 1" $ require ":=" ":= 1" @?= Just(":=","1")
    , testCase "error" $ evaluate (require "else" "then") `shouldThrow` anyException
  ]

-- Tests for Parser.-#
bindBoardTest :: TestTree
bindBoardTest = testGroup "All unit tests for -#"
  [
      testCase "" $ (accept "Lund" -# word) "Lund University" @?= Just ("University", "")
    , testCase "" $ (accept "Gote" -# word) "Lund University" @?= Nothing
    , testCase "" $ (word -# (accept "Nej")) "Lund University" @?= Nothing
    , testCase "given case" $ (accept "read" -# word) "read count" @?= Just("count","")
    , testCase "Lennart 1" $ (char -# char) "abc" @?= Just('b', "c")
    , testCase "Lennart 2" $ (char -# char) "a" @?= Nothing
  ]

-- Tests for Parser.#-
boardBindTest :: TestTree
boardBindTest = testGroup "All unit tests for #-"
  [
      testCase "" $ (accept "Lund" #- word) "Lund University" @?= Just ("Lund", "")
    , testCase "" $ (accept "Gote" #- word) "Lund University" @?= Nothing
    , testCase "" $ (word #- (accept "Nej")) "Lund University" @?= Nothing
  ]

parserTests :: TestTree
parserTests = testGroup "All Parser.hs tests"
  [
      letterTest
    , spacesTest
    , charsTest
    , requireTest
    , bindBoardTest
    , boardBindTest
  ]

-----------------------------------------------
dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.empty

testValue string = value (fromString string) dict

-- Tests for Expr.value
valueTest :: TestTree
valueTest = testGroup "All unit tests for value"
  [
      testCase "Single value" $ testValue "1" @?= 1
    , testCase "Single variable" $ testValue "x" @?= 1
    , testCase "Simple add" $ testValue "1+3" @?= 4
    , testCase "Simple sub" $ testValue "2-10" @?= (-8)
    , testCase "Simple div" $ testValue "21/7" @?= 3
    , testCase "Simple mult" $ testValue "3*7" @?= 21
    , testCase "Add two variables" $ testValue "x+y" @?= 3
    , testCase "Subtract variables" $ testValue "x-y-y" @?= (-3)
    , testCase "Division by 0" $ evaluate (testValue "1/(2-y)") `shouldThrow` anyException
    , testCase "Undefined variable" $ evaluate (testValue "2+z") `shouldThrow` anyException
  ]

exprTests :: TestTree
exprTests = testGroup "All Expr.hs tests"
  [
      valueTest
  ]

------------------------------------------------------
s9 = "while n do begin fac:=fac*n; n:=n-1; end"
parseTest :: TestTree
parseTest = testGroup "all unit tests for Task 3.b"
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
    , testCase "Begin simple inc" $ toString (fromString "begin x:=0; x:=x+1; end" :: Statement.T) @?= "begin\n\tx := 0;\n\tx := x+1;\nend\n"
    , testCase "Simple faculty" $ toString (fromString s9 :: Statement.T) @?= "while n do\n\tbegin\n\t\tfac := fac*n;\n\t\tn := n-1;\n\tend\n"
    , testCase "Read faculty" $ toString (fromString  ("begin read n; fac:=1; " ++ s9 ++ " write fac; end") :: Statement.T) @?= "begin\n\tread n;\n\tfac := 1;\n\twhile n do\n\t\tbegin\n\t\t\tfac := fac*n;\n\t\t\tn := n-1;\n\t\tend\n\twrite fac;\nend\n"
    , testCase "Parse comment" $ toString (fromString "-- Edward Blom\n" :: Statement.T) @?= "-- Edward Blom\n"
  ]

statementTest :: TestTree
statementTest = testGroup "All unit tests for Statement.hs"
  [
      parseTest
  ]

------------------------------------------------------------

allTests = testGroup "All tests"
  [
      parserTests
    , exprTests
    , statementTest
  ]

main = defaultMain allTests
