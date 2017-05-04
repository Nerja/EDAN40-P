module Lib
    (
        similarityScore
      , maximaBy
      , optAlignments
      , outputOptAlignments
      , similarityScore'
      , optAlignments'
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

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
  let alignments = optAlignments string1 string2
  putStrLn ("\nThere are " ++ show (length alignments) ++ " optimal alignments:\n")
  sequence_ [putStrLn (intersperse ' ' a ++ "\n" ++ intersperse ' ' b ++ "\n") | (a, b) <- alignments]
  putStrLn ("There were " ++ show (length alignments) ++ " optimal alignments!")

similarityScore' :: String -> String -> Int
similarityScore' xs ys = simScore (length xs) (length ys)
  where
    simScore i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]

    simEntry :: Int -> Int -> Int
    simEntry i 0 = (*) scoreSpace i
    simEntry 0 j = (*) scoreSpace j
    simEntry i j = maximum [    (+) (score x y) $ simScore (i-1) (j-1)
                              , (+) scoreSpace $ simScore i (j-1)
                              , (+) scoreSpace $ simScore (i-1) j
                            ]
      where
        x = xs!!(i-1)
        y = ys!!(j-1)

optAlignments' :: String -> String -> [AlignmentType]
optAlignments' xs ys = map (\(x,y) -> (reverse x, reverse y)) $ optAl (length xs) (length ys)
  where
    optAl i j = optTable!!i!!j
    optTable = [[ optEntry i j | j<-[0..]] | i<-[0..] ]

    optEntry :: Int -> Int -> [AlignmentType]
    optEntry 0 0 = [([], [])]
    optEntry i 0 = attachHeads (xs!!(i-1)) '-' $ optAl (i-1) 0
    optEntry 0 j = attachHeads '-' (ys!!(j-1)) $ optAl 0 (j-1)
    optEntry i j = maximaBy (uncurry stringScore) (    attachHeads x y (optAl (i-1) (j-1))
                                                    ++ attachHeads x '-' (optAl (i-1) j)
                                                    ++ attachHeads '-' y (optAl i (j-1))
                                                  )
      where
        x = xs!!(i-1)
        y = ys!!(j-1)
