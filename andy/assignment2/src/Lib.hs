module Lib
    (
        similarityScore
      , maximaBy
    ) where

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

similarityScore :: String -> String -> Int
similarityScore [] ys = (*) scoreSpace $ length ys
similarityScore xs [] = (*) scoreSpace $ length xs
similarityScore (x:xs) (y:ys) = maximum [   (+) (score x y) $ similarityScore xs ys
                                          , (+) scoreSpace $ similarityScore xs (y:ys)
                                          , (+) scoreSpace $ similarityScore (x:xs) ys
                                        ]

score :: Char -> Char -> Int
score x y
  | x == '-' || y == '-' = scoreSpace
  | x == y               = scoreMatch
  | otherwise            = scoreMismatch

-- Attaches the first argument to first element of the tuple and
-- the second argument to second element of the tuple at the head of the list in
-- every tuple presented in the list of tuples (third argument)
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [x | x <- xs, valueFcn x == (maximum $ map valueFcn xs)]
