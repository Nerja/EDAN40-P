import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec
import Control.Exception
import Parser
import Dictionary
import Expr
import Statement
import Program


p = ("\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end")

pComment = ("\
\read k;\
\read n;\
\-- Hi, Hej\n\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      -- Skip same as just comment\n\
\    else\
\      write m;\
\    m := m + 1;\
\  end")

pPretty = ""

p1 = ("\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\
\    p := p*10;\
\    n :=q;\
\  end\
\write s;")

p4 = ("\
\read a;\
\read b;\
\-- a comment\n\
\s := 3;\
\while a do\
\  begin\
\    c := a^s;\
\    d := 2^a;\
\    write c;\
\    write d;\
\    a := a-1;\
\  end\
\write a;")

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

exprTests :: TestTree
exprTests = testGroup "All Expr.hs tests"
  [
      valueTest
    , parseExprTest
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
    , testCase "Comment with skip" $ toString (fromString "begin -- Edward Blom \nskip; end" :: Statement.T) @?= "begin\n\t-- Edward Blom \n\tskip;\nend\n"
  ]

statementTest :: TestTree
statementTest = testGroup "All unit tests for Statement.hs"
  [
      parseTest
  ]

------------------------------------------------------------
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

--------------

allTests = testGroup "All tests"
  [
      parserTests
    , exprTests
    , statementTest
    , programTest
  ]

main = defaultMain allTests
