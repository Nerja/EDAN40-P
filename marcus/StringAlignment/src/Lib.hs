module Lib
(
    similarityScore
  , maximaBy
  , optAlignments
  , outputOptAlignments
) where

import Utils
import Data.List

-- | Returns the score of the optimal alignment of two strings.
--   scoreMatch, scoreMisMatch, scoreSpace is the used parameters
--   for measuring "optimalness". The cost parameters are given
--   as the first three arguments in the order scoreMatch, scoreMismatch,
--   scoreSpace.
--   Examples:
--
--   >>> similarityScore 1 (-1) (-2) "HASKELL" "PASCAL"
--   -2
similarityScore :: Int -> Int -> Int -> String -> String -> Int
similarityScore a b c [] t = (*) c $ length t
similarityScore a b c s [] = (*) c $ length s
similarityScore a b c alls@(s:ss) allt@(t:ts) = maximum [keepLetters, spaceS, spaceT]
    where keepLetters = score a b c s t + similarityScore a b c ss ts
          spaceS      = c + similarityScore a b c alls ts
          spaceT      = c + similarityScore a b c ss allt

-- | Returns the score associated by matching the characters x and y.
--   The matching parameters(scoreMatch, scoreMisMatch, scoreSpace) are
--   supplied to the function.
--
--   Examples:
--
--   >>> score 1 2 3 '-' 'a'
--   3
--   >>> score 1 2 3 'a' 'b'
--   2
--   >>> score 1 2 3 'a' 'a'
--   1
score :: Int -> Int -> Int -> Char -> Char -> Int
score a b c x y | x == '-' || y == '-'  = c
                | x == y     = a
                | otherwise  = b

-- | Takes two elements and a list of pairs where each pair consists of two lists.
--   The first element is prepended to the first list of all pairs. The second
--   element is prepended to the second list of all pairs. The modified pairs
--   are returned as a list.
--
--   Examples:
--
--   >>> attachHeads 1 2 [([], []), ([], [])]
--   [([1],[2]),([1],[2])]
--   >>> attachHeads 'a' 'b' [([],[]),("pple","ike")]
--   [("a","b"),("apple","bike")]
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- | Takes a list of elements and returns all the "maximum" elements.
--   The elements are compared using the provided function.
--
--   Examples:
--
--   >>> maximaBy length ["hej", "fisk", "ksif", "he"]
--   ["fisk","ksif"]
maximaBy :: (Ord b) => (a -> b) -> [a] -> [a]
maximaBy f el = map fst filterAnot
  where anotPairs   = zip el $ map f el
        maximumVal  = maximum $ map snd anotPairs
        filterAnot  = filter ((==maximumVal).snd) anotPairs

type AlignmentType = (String, String)
-- | Takes two strings s and t and returns all optimal alignments of
--   the two strings. The cost parameters are given
--   as the first three arguments in the order scoreMatch, scoreMismatch,
--   scoreSpace.
--
--   Examples:
--
--   >>> optAlignments 0 (-1) (-1) "writers" "vintner"
--   [("writ-ers","vintner-"),("wri-t-ers","v-intner-"),("wri-t-ers","-vintner-")]
optAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optAlignments _ _ _ [] t = [(replicate (length t) '-', t)]
optAlignments _ _ _ s [] = [(s, replicate (length s) '-')]
optAlignments a b c alls@(s:ss) allt@(t:ts) = maximaBy compScore alignments
  where alignments  = keepLetters ++ spaceS ++ spaceT
        keepLetters = attachHeads s t $ optAlignments a b c ss ts
        spaceS      = attachHeads '-' t $ optAlignments a b c alls ts
        spaceT      = attachHeads s '-' $ optAlignments a b c ss allt
        compScore   = uncurry $ (sum .) . zipWith (score a b c)

-- | Takes two strings s and t and returns all optimal alignments of
--   the two strings. The cost parameters are given
--   as the first three arguments in the order scoreMatch, scoreMismatch,
--   scoreSpace. The optimal alignments are returned as a string.
outputOptAlignments :: Int -> Int -> Int -> String -> String -> String
outputOptAlignments = ((((concatMap alignStr. ) .) .) .) . optAlignments
  where alignStr (a, b) =  (intersperse ' ' a) ++ "\n" ++ (intersperse ' ' b) ++ "\n" ++ "\n"
