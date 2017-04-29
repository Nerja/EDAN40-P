module Lib
(
    similarityScore
  , maximaBy
  , optAlignments
  , outputOptAlignments
  , similarityScore'
  , optAlignments'
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
maximaBy f el = filter ((==maximumVal).f) el
  where maximumVal  = maximum $ map f el

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

-- Part 3
-- Same as similarityScore just faster
similarityScore' :: Int -> Int -> Int -> String -> String -> Int
similarityScore' a b c ss ts = simScore (length ss) (length ts)
  where simScore i j = simTable!!i!!j
        simTable = [[simEntry i j | j <- [0..]] | i <- [0..]]

        simEntry :: Int -> Int -> Int
        simEntry 0 j = c * j
        simEntry i 0 = c * i
        simEntry i j  = maximum [keepLetters, spaceS, spaceT]
          where keepLetters     = score a b c s t + simScore (i-1) (j-1)
                spaceS          = c + simScore i (j-1)
                spaceT          = c + simScore (i-1) j
                s               = ss!!(length ss - i)
                t               = ts!!(length ts - j)

-- Same as optAlignments just faster
optAlignments' :: Int -> Int -> Int -> String -> String -> [AlignmentType]
optAlignments' a b c ss ts = map snd $ optAl (length ss) $ length ts
  where optAl i j = optTable!!i!!j
        optTable = [[optEntry i j | j <- [0..]] | i <- [0..]]

        takeTail k s = drop (length s - k) s
        optEntry :: Int -> Int -> [(Int, AlignmentType)]
        optEntry 0 j = [(c*j, (replicate j '-', takeTail j ts))]
        optEntry i 0 = [(c*i, (takeTail i ss, replicate i '-'))]
        optEntry i j = maximaBy fst (keepLetters ++ spaceS ++ spaceT)
          where keepLetters     = map (\(rs, (p,q)) -> (rs + score a b c s t, (s:p, t:q))) $ optAl (i-1) (j-1)
                spaceS          = map (\(rs, (p,q)) -> (rs + c, ('-':p, t:q))) $ optAl i (j-1)
                spaceT          = map (\(rs, (p,q)) -> (rs + c, (s:p, '-':q))) $ optAl (i-1) j
                s               = ss!!(length ss - i)
                t               = ts!!(length ts - j)
