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
    Write Expr.T |
    Comment String
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

parseComment = accept "--" -# iter (char ? (/= '\n')) #- require "\n" >-> Comment

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

exec (Comment _ : stmts) dict input = exec stmts dict input

exec [] _ _ = []

-- | Takes an indention level and a statement and returns a string
--   representing the statement.
stringify :: String -> Statement -> String
stringify ind (Assignment str e) = ind ++ str ++ " := " ++ toString e ++ ";" ++ "\n"
stringify ind (Skip) = ind ++ "skip;" ++ "\n"
stringify ind (Read name) = ind ++ "read " ++ name ++ ";" ++ "\n"
stringify ind (Write e) = ind ++ "write " ++ toString e ++ ";" ++ "\n"
stringify ind (If e th el) = ind ++ "if " ++ toString e ++ " then\n" ++ stringify (ind++"\t") th ++ ind ++ "else\n" ++ stringify (ind++"\t") el
stringify ind (While e st) = ind ++ "while " ++ toString e ++ " do\n" ++ stringify (ind++"\t") st
stringify ind (Statements xs) = ind ++ "begin\n" ++ concatMap (stringify (ind++"\t")) xs ++ ind ++ "end\n"
stringify ind (Comment sen) = ind ++ "-- " ++ sen ++ "\n"

tryAllParsers = parseAssignment ! parseSkip ! parseRead ! parseWrite ! parseIf ! parseWhile ! parseBeginEnd ! parseComment
instance Parse Statement where
  parse = tryAllParsers
  toString = stringify ""
