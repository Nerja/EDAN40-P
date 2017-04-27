module Lib
    (   scoreMatch
      , scoreMismatch
      , scoreSpace
      , similarityScore
    ) where

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

similarityScore :: String -> String -> Int
similarityScore [] ys = (*) scoreSpace $ length ys
similarityScore xs [] = (*) scoreSpace $ length xs
similarityScore (x:xs) (y:ys) = maximum [(+) (similarityScore xs ys) $ score x y, (+) scoreSpace $ similarityScore xs (y:ys), (+) scoreSpace $ similarityScore (x:xs) ys]

score :: Char -> Char -> Int
score x y
  | x == '-' || y == '-' = scoreSpace
  | x == y               = scoreMatch
  | otherwise            = scoreMismatch
