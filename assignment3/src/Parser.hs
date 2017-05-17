module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons(a, b) = a:b

-- | Takes two parsers and applies them sequentially. Returns the result from
--   the application of the second parser.
--
--   Examples:
--
--   >>> (word -# word) "Lund University"
--   Just ("University","")
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

-- | Takes two parsers and applies them sequentially. Returns the result from
--   the application of the first parser.
--
--   Examples:
--
--   >>> (word #- word) "Lund University"
--   Just ("Lund","")
(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

-- | Parser that accepts any number of whitespaces
--
--   Examples:
--
--   >>> spaces " \nHejsan"
--   Just (" \n","Hejsan")
spaces :: Parser String
spaces = iter $ char ? isSpace

token :: Parser a -> Parser a
token m = m #- spaces

-- | Parser for parsing a single letter.
--
--   Examples:
--
--   >>> letter "HEJ"
--   Just ('H',"EJ")
--   >>> letter "123"
--   Nothing
letter :: Parser Char
letter =  char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

-- | Parser accepting a given amount of chars
--
--   Examples:
--
--   >>> chars 6 "HejsanEdwardBlom"
--   Just ("Hejsan","EdwardBlom")
--   >>> chars 100 "HejsanEdwardBlom"
--   Nothing
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- | Parser that accepts the given string. In case of failure
--   uses err to report the failure.
require :: String -> Parser String
require w  = accept w ! err ("expecting: " ++ w)

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')
