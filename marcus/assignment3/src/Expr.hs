module Expr(Expr, T, parse, fromString, value, toString) where

{-
   An expression of type Expr is a representation of an arithmetic expression
   with integer constants and variables. A variable is a string of upper-
   and lower case letters. The following functions are exported

   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int

   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.

   fromString expects its argument to contain an expression and returns the
   corresponding Expr.

   toString converts an expression to a string without unneccessary
   parentheses and such that fromString (toString e) = e.

   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.
-}
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import Data.Maybe

data Expr = Num Integer | Var String | Add Expr Expr
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
       | Pow Expr Expr
         deriving Show

type T = Expr

var, num, factor, term, expr :: Parser Expr

term', expr' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> const Mul !
        lit '/' >-> const Div

addOp = lit '+' >-> const Add !
        lit '-' >-> const Sub

bldOp e (oper,e') = oper e e'

powOp = lit '^' >-> const Pow

factor = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"

term' e = mulOp # powTerm >-> bldOp e #> term' ! return e
term = powTerm #> term'

powTerm' e = powOp # factor >-> bldOp e ! return e
powTerm = factor #> powTerm'

expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)
shw prec (Pow t u) = parens (prec>7) (shw 8 t ++ "^" ++ shw 7 u)

-- | Takes an expression and a dictionary with variable values
--   and evaluates the expression.
value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _ = n
value (Var name) dict = fromMaybe (error $ "Expr.value: undefined variable " ++ name) $ Dictionary.lookup name dict
value (Add e1 e2) dict = value e1 dict + value e2 dict
value (Sub e1 e2) dict = value e1 dict - value e2 dict
value (Div e1 e2) dict = if (e2val == 0) then error "Expr.value: division by 0" else value e1 dict `quot` e2val
  where e2val = value e2 dict
value (Mul e1 e2) dict = value e1 dict * value e2 dict
value (Pow b e) dict = value b dict ^ value e dict

instance Parse Expr where
    parse = expr
    toString = shw 0
