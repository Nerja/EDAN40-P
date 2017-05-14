module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Statements [Statement] |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Comment String
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifStmt = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e, t), s) = If e t s

skip = accept "skip;" >-> const Skip

statements = accept "begin" -# iter parse #- require "end" >-> Statements

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

readStmt = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

comment = accept "--" -# iter (char ? (/= '\n')) #- require "\n" >-> Comment

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment v e: stmts) dict input = exec stmts (Dictionary.insert (v, Expr.value e dict) dict) input

exec (If cond thenStmts elseStmts: stmts) dict input =
    if Expr.value cond dict > 0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (Skip: stmts) dict input = exec stmts dict input

exec (Statements ss: stmts) dict input = exec (ss ++ stmts) dict input

exec (While cond s: stmts) dict input =
  if Expr.value cond dict > 0
  then exec (s: While cond s: stmts) dict input
  else exec stmts dict input

exec (Read v: stmts) dict input = exec stmts (Dictionary.insert (v, head input) dict) $ tail input

exec (Write e: stmts) dict input = Expr.value e dict: exec stmts dict input

exec (Comment c: stmts) dict input = exec stmts dict input

stringify :: String -> Statement -> String
stringify ind (Assignment v e) = ind ++ v ++ " := " ++ toString e ++ ";\n"
stringify ind (If e t s) = ind ++ "if " ++ toString e ++ " then\n" ++ stringify (ind ++ "\t") t
                           ++ ind ++ "else\n" ++ stringify (ind ++ "\t") s
stringify ind Skip = ind ++ "skip;\n"
stringify ind (Statements ss) = ind ++ "begin\n" ++ concatMap (stringify (ind ++ "\t")) ss
                                ++ ind ++ "end\n"
stringify ind (While e s) = ind ++ "while " ++ toString e ++ " do\n" ++ stringify (ind ++ "\t") s
stringify ind (Read v) = ind ++ "read " ++ v ++ ";\n"
stringify ind (Write e) = ind ++ "write " ++ toString e ++ ";\n"
stringify ind (Comment c) = ind ++ "-- " ++ c ++ "\n"

instance Parse Statement where
  parse = assignment ! ifStmt ! skip ! statements ! while ! readStmt ! write ! comment
  toString = stringify ""
