module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
-- Should be 7 constructors
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Skip |
    Statements [Statement] |
    Read String |
    Write Expr.T
    deriving Show

parseAssignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

parseSkip = accept "skip;" >-> const Skip
parseRead = accept "read" -# word #- require ";" >-> Read

parseWrite = accept "write" -# Expr.parse #- require ";" >-> Write

parseIf = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e,t),el) = If e t el

parseWhile = accept "while" -# Expr.parse #- require "do" # parse >-> \(e, st) -> While e st

parseBeginEnd = accept "begin" -# (iter tryAllParsers) #- require "end" >-> Statements

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment var exp : stmts) dict input = exec stmts (Dictionary.insert (var,expVal) dict) input
  where expVal = Expr.value exp dict

exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (me@(While cond whileStmts) : stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (whileStmts : me : stmts) dict input
    else exec (stmts) dict input

exec (Skip : stmts) dict input = exec stmts dict input

exec (Statements blockStmts : stmts) dict input = exec (blockStmts ++ stmts) dict input

exec (Read var : stmts) dict input = exec stmts (Dictionary.insert (var, head input) dict) $ tail input

exec (Write exp : stmts) dict input = (Expr.value exp dict) : exec stmts dict input

exec [] _ _ = []

stringify :: Statement -> String
stringify (Assignment str e) = str ++ " := " ++ toString e ++ ";"
stringify (Skip) = "skip;"
stringify (Read name) = "read " ++ name ++ ";"
stringify (Write e) = "write " ++ toString e ++ ";"
stringify (If e th el) = "if " ++ toString e ++ " then " ++ toString th ++ " else " ++ toString el
stringify (While e st) = "while " ++ toString e ++ " do " ++ toString st
stringify (Statements xs) = "begin " ++ concatMap ((++" ").toString) xs ++ "end"

tryAllParsers = parseAssignment ! parseSkip ! parseRead ! parseWrite ! parseIf ! parseWhile ! parseBeginEnd
instance Parse Statement where
  parse = tryAllParsers
  toString = stringify
