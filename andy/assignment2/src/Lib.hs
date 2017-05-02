module Lib
    (
        similarityScore
      , maximaBy
      , optAlignments
      , outputOptAlignments
    ) where

import Data.List

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

stringScore :: String -> String -> Int
stringScore [] ys = (*) scoreSpace $ length ys
stringScore xs [] = (*) scoreSpace $ length xs
stringScore (x:xs) (y:ys)
  | x == '-' || y == '-' = (+) scoreSpace $ stringScore xs ys
  | x == y               = (+) scoreMatch $ stringScore xs ys
  | otherwise            = (+) scoreMismatch $ stringScore xs ys

-- Attaches the first argument to first element of the tuple and
-- the second argument to second element of the tuple at the head of the list in
-- every tuple presented in the list of tuples (third argument)
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [x | x <- xs, valueFcn x == (maximum $ map valueFcn xs)]

type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 [] = [(string1, replicate (length string1) '-')]
optAlignments [] string2 = [(replicate (length string2) '-', string2)]
optAlignments (x:xs) (y:ys) = maximaBy (uncurry stringScore) (     attachHeads x y (optAlignments xs ys)
                                                                ++ attachHeads x '-' (optAlignments xs (y:ys))
                                                                ++ attachHeads '-' y (optAlignments (x:xs) ys)
                                                             )
                                                             
outputOptAlignments :: String -> String -> IO()
outputOptAlignments string1 string2 = do
  let alignments = optAlignments string1 string2
  putStrLn ("\nThere are " ++ show (length alignments) ++ " optimal alignments:\n")
  sequence_ [putStrLn (intersperse ' ' a ++ "\n" ++ intersperse ' ' b ++ "\n") | (a, b) <- alignments]
  putStrLn ("There were " ++ show (length alignments) ++ " optimal alignments!")
