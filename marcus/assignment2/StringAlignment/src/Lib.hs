module Lib
    (
        similarityScore
    ) where

-- Default values
scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2

-- | Returns the score of the optimal alignment of two strings.
--   scoreMatch, scoreMisMatch, scoreSpace is the used parameters
--   for measuring "optimalness"
--
--   Examples:
--
--   >>> similarityScore "HASKELL" "PASCAL"
--   -2
similarityScore :: String -> String -> Int
similarityScore [] t = (*) scoreSpace $ length t
similarityScore s [] = (*) scoreSpace $ length s
similarityScore alls@(s:ss) allt@(t:ts) = maximum [keepLetters, spaceS, spaceT]
    where keepLetters = keepScore + similarityScore ss ts
          keepScore   = if (s == t) then scoreMatch else scoreMismatch
          spaceS      = scoreSpace + similarityScore alls ts
          spaceT      = scoreSpace + similarityScore ss allt

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
