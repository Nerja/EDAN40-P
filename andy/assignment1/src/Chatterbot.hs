module Chatterbot where
import Utilities
import System.Random
import Data.Char

import Data.Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
{- TO BE WRITTEN -}
stateOfMind brain = do
  r <- randomIO :: IO Float
  return (rulesApply (map (map2 (id, pick r)) brain))

rulesApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply = (fromMaybe [] .) . transformationsApply "*" reflect

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = map (try (`lookup` reflections))

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
{- TO BE WRITTEN -}
rulesCompile = (map . map2) (words . map toLower, map (words . map toLower))


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply = fix . try . transformationsApply "*" id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wildcard (t:ts) s
  | wildcard == t    = s ++ substitute wildcard ts s
  | otherwise = t: substitute wildcard ts s
{- TO BE WRITTEN -}


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ _ [] = Nothing
match _ [] _ = Nothing
match wildcard (p:ps) (s:ss)
  | p /= wildcard && p == s = match wildcard ps ss
  | p == wildcard           = orElse (singleWildcardMatch (p:ps) (s:ss))
      (longerWildcardMatch (p:ps) (s:ss))
  | otherwise = Nothing
{- TO BE WRITTEN -}


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)
{- TO BE WRITTEN -}
longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)
{- TO BE WRITTEN -}



-- Test cases --------------------

--testPattern =  "a=*;"
--testSubstitutions = "32"
--testString = "a=32;"

--substituteCheck = substituteTest == testString

--matchTest = match '*' testPattern testString
--matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc func input (a, b) = mmap (substitute wc b . func) (match wc a input)
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wc func xs input = foldl1 orElse (map (transformationApply wc func input) xs)
{- TO BE WRITTEN -}
